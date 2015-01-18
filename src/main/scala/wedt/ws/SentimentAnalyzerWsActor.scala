package wedt.ws

import akka.actor.Actor
import com.typesafe.scalalogging.slf4j.StrictLogging
import org.jsoup.nodes.Document
import spray.http.{MediaTypes, StatusCodes}
import spray.json.DefaultJsonProtocol
import spray.routing._
import spray.util.LoggingContext
import wedt.analyzer._
import wedt.conf.Config
import wedt.crawler.ReviewsFinder.ReviewParams
import wedt.crawler.{WebsiteToXml, NextPageFinder, SupportedLanguages}
import wedt.di.WedtModule

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success}

//import scalaz._
//import Scalaz._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
class SentimentAnalyzerWsActor extends Actor with SentimentAnalyzerWs with WedtModule {
  def receive = runRoute(queryRoute)
}

object JsonImplicits extends DefaultJsonProtocol {
  implicit val impAnalyzeRequest = jsonFormat4(AnalyzeRequest)
  implicit val impPost = jsonFormat3(Post)
  implicit val impAnalyzeResponse = jsonFormat3(AnalyzeResponse)
}

case class RawWebsite(url: String, html: String) {
  lazy val baseUrl = wedt.crawler.UrlToBaseUrl.toBaseUrl(url)
}

case class AnalyzeRequest(url: String, minimumReviews: Option[Int], minimumWordsInReview: Option[Int], maxPages: Option[Int])

case class Analysis(request: AnalyzeRequest, lang: SupportedLanguages.Value)



case class AnalyzeResponse(request: AnalyzeRequest, posts: List[Post], overallSentiment: Double)

trait SentimentAnalyzerWs extends HttpService with StrictLogging {
  this: WedtModule =>
  override def actorRefFactory = actorSystem
  implicit val system = actorSystem
  import system.dispatcher // execution context for futures

  import spray.httpx.SprayJsonSupport.{sprayJsonMarshaller, sprayJsonUnmarshaller}
  import wedt.ws.JsonImplicits._

  implicit def myExceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e => requestUri { uri =>
        log.error("Got error", e)
        complete(400, e.printStackTrace)
      }

    }



  def findNextPageLink(doc: Document, baseUrl: String, url: String, lang: SupportedLanguages.Value): Option[String] = {
    NextPageFinder.findNextPageLink(doc, baseUrl, url, lang) match {
      case Some(nextPageLink) => {
        logger.info(s"Found link to next page: $nextPageLink")
        Some(nextPageLink)
      }
      case None => {
        logger.info(s"Could not find link to next page for url: $url")
        None
      }
    }
  }

  def fetchPages(doc: Document, baseUrl: String, url: String, lang: SupportedLanguages.Value, numberOfPages: Int): Future[List[Document]] = {
    if (numberOfPages > 0) {
      findNextPageLink(doc, baseUrl, url, lang) match {
        case Some(nextUrl) =>

          val absUrl = NextPageFinder.toAbsoluteUrl(baseUrl, nextUrl)

          httpClient.get(absUrl).flatMap(html => {
            logger.info(s"Got html for next page with url: $absUrl")
            logger.debug(html)

            WebsiteToXml.toJsoupDoc(html) match {
              case Left(err) => Future.successful(List())
              case Right(nextDoc) => fetchPages(nextDoc, baseUrl, url, lang, numberOfPages - 1).map(x => nextDoc :: x)
            }
          })

        case None => Future.successful(List())
      }
    }
    else Future.successful(List())
  }


  val queryHandler: (Analysis) => Future[Either[String, AnalyzeResponse]] = (analysis) => {

    val url = analysis.request.url
    val lang = analysis.lang
    logger.info(s"Got request to analyze url: $url for language: ${analysis.lang}")

    import wedt.crawler._

    httpClient.get(url).flatMap( html => {
      logger.info(s"Got html response for url: $url")
      logger.debug(html)
      try {
        WebsiteToXml.toJsoupDoc(html) match {
          case Left(err) => Future.successful(Left(err))
          case Right(doc) =>
            val baseUrl = UrlToBaseUrl.toBaseUrl(url)
            val params = ReviewParams(analysis.request.minimumReviews.getOrElse(Config.crawler.minimumPosts),
              analysis.request.minimumWordsInReview.getOrElse(Config.crawler.minimumWords))

            // TODO request has timeout so I doubt we could visit all 100 pages of reviews,
            // Plus there is risk of being banned so there is configurable cap of maximal number of pages.
            val pagesToAnalyze = analysis.request.maxPages.getOrElse(Config.crawler.pagesToAnalyze)

            val maybeSomeReviews = ReviewsFinder.findReviews(doc, lang, params)

            if(maybeSomeReviews.nonEmpty) {

              val allPagesToAnalyzeF = fetchPages(doc, baseUrl, url, lang, pagesToAnalyze).map {
                case Nil => logger.info("Could not retrieve any more pages besides starting one")
                  List(doc)
                case restOfPages => {
                  logger.info(s"Retireved ${restOfPages.size} pages")
                  doc :: restOfPages
                }
              }

              val postsF = allPagesToAnalyzeF.map(allPages => {
                val allReviews = allPages.map(page => ReviewsFinder.findReviews(doc, lang, params)).flatten
                val stemmer = Stemmers.getStemmer(lang) // creating stemmer per request because they are not thread safe
                val dictionary = Dictionaries.getDictionary(lang)
                val analyzer = SentimentAnalyzer.analyzer(dictionary, stemmer, lang)

                val analyzedReviews = allReviews.map(review => {
                  analyzer(review.review)
                })

                val analyzedSimple = analyzedReviews.map(_.toSimpleForm)
                val total = SentimentAnalyzer.totalWords(analyzedSimple)

                val analyzedPosts = analyzedSimple.map(review => {
                  Post(review.review, SentimentAnalyzer.calculateSentiment(review), SentimentAnalyzer.relevance(review, total))
                })

                val overAllSentiment = SentimentAnalyzer.overallSentiment(analyzedPosts)

                (analyzedPosts, overAllSentiment)
              })

              postsF.map(posts => {
                val either: Either[String, AnalyzeResponse] = Right(AnalyzeResponse(analysis.request, posts._1, posts._2))
                either
              })
            }
            else {
              Future.successful(Left(
                s"""
                 |Could not detect any reviews on site ${url} or there are no sufficient number of reviews.
                 |Requires ${params.minimumReviews} minimum reviews and each review should have minimum of ${params.minimumWordsInReview} words.
               """.stripMargin))
            }
        }
      } catch {
        case e =>
          logger.error("Error occured", e)
          Future.successful(Left("Error occured " + e.getMessage))
      }

    })

  }

  val infoRoute = get {
    respondWithMediaType(MediaTypes.`text/html`) { ctx =>

      ctx.complete(
        s"""
<!DOCTYPE html>
<html>
<head lang="en">
    <meta charset="UTF-8">
    <title></title>
</head>
<body>
<h1> welcome</h1>
<pre>For english analysis POST JSON to ${Config.restApi.context}/${Config.restApi.queryPath}?lang=EN</pre>
<pre>For polish analysis POST JSON to ${Config.restApi.context}/${Config.restApi.queryPath}?lang=PL</pre>

<div>
  <p>Example json</p>

  <pre>
      { "url": "http://cokupic.pl/produkt/Lark-FreeBird-35AT-35-LarkMap-Polska" }
  </pre>

  <pre>
      { "url": "http://cokupic.pl/produkt/Lark-FreeBird-35AT-35-LarkMap-Polska",
        "minimumReviews": 3,
        "minimumWordsInReview": 10,
        "maxPages": 5
       }
  </pre>
</div>
</body>
</html>
         """)

    }
  }

  val queryRoute: Route = path(Config.restApi.context / Config.restApi.queryPath) {
    post {
      entity(as[AnalyzeRequest]) { raw =>
        anyParam('lang)(lang => {
          val chosenLanguage = SupportedLanguages.withName(lang)
          onComplete(queryHandler(Analysis(raw, chosenLanguage))) { response => {
            response match {
              case Success(either) => either match {
                case Left(error) => respondWithStatus(StatusCodes.BadRequest) { ctx =>
                  ctx.complete(s"An error occurred: ${error}")
                }
                case Right(res) => respondWithMediaType(MediaTypes.`application/json`) { ctx =>
                  ctx.complete(res)
                }
              }
              case Failure(ex) => respondWithStatus(StatusCodes.BadRequest) { ctx =>
                ctx.complete(s"An error occurred: ${ex.getMessage}")
              }
            }
          }
          }
        })
      }
    }
  } ~ infoRoute

}
