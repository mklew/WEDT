package wedt.ws

import akka.actor.Actor
import spray.http.{MediaTypes, StatusCodes}
import spray.json.DefaultJsonProtocol
import spray.routing._
import wedt.conf.Config
import wedt.di.WedtModule

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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
  implicit val impAnalyzeRequest = jsonFormat1(AnalyzeRequest)
  implicit val impPost = jsonFormat3(Post)
  implicit val impAnalyzeResponse = jsonFormat2(AnalyzeResponse)
}

case class AnalyzeRequest(url: String)

case class Post(text: String, sentiment: Double, relevance: Double)

case class AnalyzeResponse(request: AnalyzeRequest, posts: Seq[Post])

trait SentimentAnalyzerWs extends HttpService {
  this: WedtModule =>
  override def actorRefFactory = actorSystem

  import spray.httpx.SprayJsonSupport.{sprayJsonMarshaller, sprayJsonUnmarshaller}
  import wedt.ws.JsonImplicits._

  val queryRoute: Route = path(Config.restApi.context / Config.restApi.queryPath) {
    post {
      entity(as[AnalyzeRequest]) { raw =>
        onComplete(queryHandler(raw)) { response => {
          response match {
            case Success(either) => either match {
              case Left(error) => respondWithStatus(StatusCodes.BadRequest) { ctx =>
                ctx.complete(s"An error occurred: ${error}")
              }
              case Right(response) => respondWithMediaType(MediaTypes.`application/json`) { ctx =>
                ctx.complete(response)
              }
            }
            case Failure(ex) => respondWithStatus(StatusCodes.BadRequest) { ctx =>
              ctx.complete(s"An error occurred: ${ex.getMessage}")
            }
          }
        }
        }
      }
    }
  }


  val queryHandler: (AnalyzeRequest) => Future[Either[String, AnalyzeResponse]] = ???
}
