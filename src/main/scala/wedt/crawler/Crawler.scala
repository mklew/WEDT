package wedt.crawler

import java.net.URL
import java.util.Locale

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.jsoup.Jsoup

import scala.util.{Failure, Success, Try}
import org.jsoup.nodes.{Element, Document}
import scala.collection.JavaConversions._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object Crawler {

}

object SupportedLanguages extends Enumeration {
  type Lang = Value
  val PL, EN = Value

  def getLocale(v: Value) = {
    v match {
      case PL => new java.util.Locale("pl_PL")
      case EN => Locale.ENGLISH
    }
  }
}

object WebsiteToXml extends StrictLogging {

  @deprecated("use jsoup")
  def toXml(raw: String): Either[String, scala.xml.Elem] = {
    Try(scala.xml.XML.loadString(raw)) match {
      case Success(xml) => Right(xml)
      case Failure(err) => {
        logger.error("Failed to transform website to XML", err)
        Left(err.getMessage)
      }
    }
  }

  def toJsoupDoc(rawHtml: String, baseUrl: String): Either[String, Document] = {
    Try(Jsoup.parse(rawHtml, baseUrl)) match {
      case Success(doc) => Right(doc)
      case Failure(err) => {
        logger.error("Failed to transform website to Jsoup document", err)
        Left(err.getMessage)
      }
    }
  }

  def toJsoupDoc(rawHtml: String): Either[String, Document] = {
    Try(Jsoup.parse(rawHtml)) match {
      case Success(doc) => Right(doc)
      case Failure(err) => {
        logger.error("Failed to transform website to Jsoup document", err)
        Left(err.getMessage)
      }
    }
  }
}

object UrlToBaseUrl {
  def toBaseUrl(url: String): String = {
    val value = new URL(url)

    println(
      s"""
        | host = ${value.getHost}
        | port = ${value.getPort}
        | query = ${value.getQuery}
        | path = ${value.getPath}
        | protocol = ${value.getProtocol}
        | ref = ${value.getRef}
      """.stripMargin)

    s"${value.getProtocol}://${value.getHost}/"
  }
}


case class Review(review: String)
case class ReviewParams(minimumReviews: Int, minimumWordsInReview: Int)

trait ReviewsFinder {
  type Reviews = Seq[Review]

  def findReviews(htmlDoc: Document, lang: SupportedLanguages.Lang, params: ReviewParams): Reviews
}

object NextPageFinder {

  private val langToKeyWords = Map(
    SupportedLanguages.PL -> Seq("następna", "nastepny", "nastepna", "dalej", "przejdź"),
    SupportedLanguages.EN -> Seq("next")
  )

  private val attributesToCheckForKeywords = List("title", "alt")
  private val cssClasses = Seq("next", "next_page", "pagination")

  /**
   * Unfortunately html is rarely valid xml so we will be working with raw strings
   * @param htmlDoc - jsoup document
   */
  protected[crawler] def findAllLinks(htmlDoc: Document): Seq[Element] = {
    htmlDoc.select("a").filter(_.hasAttr("href"))
  }

  protected[crawler] def findLinksWithRelNext(htmlDoc: Document): Seq[Element] = {
    htmlDoc.select("""a[rel="next"]""").filter(_.hasAttr("href"))
  }

  protected[crawler] def findLinksWhichMatchAttributesAndValues(htmlDoc: Document, attr: String, value: String): Seq[Element] = {
    htmlDoc.select(s"""a[$attr~="$value"]""").filter(_.hasAttr("href"))
  }

  private def isRelative(href: String) = href.startsWith("/")

  protected[crawler] def filterToOnlyThoseWithBaseUrl(baseUrl: String)(element: Element): Boolean = {
    val href = element.attr("href")
    isRelative(href) || href.startsWith(baseUrl)
  }

  def toAbsoluteUrl(baseUrl: String, maybeRelative: String) = if(isRelative(maybeRelative)) joinWithRelative(baseUrl, maybeRelative) else maybeRelative

  protected[crawler] def joinWithRelative(baseUrl: String, relative: String): String = {
    if(baseUrl.endsWith("/") && relative.startsWith("/")) {
      baseUrl + relative.tail
    }
    else if(baseUrl.endsWith("/")) {
      baseUrl + relative
    }
    else if(relative.startsWith("/")) {
      baseUrl + relative
    }
    else baseUrl + "/" + relative
  }

  protected[crawler] def longestPrefix(baseUrl: String, url: String)(element: Element): String = {
    require(element.hasAttr("href"))
    val href = element.attr("href")

    val prefixes = for {
      length <- 1 to url.length
    } yield url.take(length)

    val absoluteHref = if(isRelative(href)) {
      joinWithRelative(baseUrl, href)
    }
    else href

    prefixes.filter(p => absoluteHref.startsWith(p)).maxBy(_.length)
  }

  protected[crawler] def orderByLongestPrefix(baseUrl: String, url: String)(elements: Seq[Element]): Seq[Element] = {
    elements.map(e => e -> longestPrefix(baseUrl, url)(e)).toSeq.sortBy(_._2.length).reverse.map(_._1)
  }

  case class LinkFindingParameters(locale: Locale, keywords: Seq[String])

  def findNextPageLink(htmlDoc: Document, baseUrl: String, url: String, lang: SupportedLanguages.Value): Option[String] = {
    findPotentialNextPageLinks(htmlDoc, baseUrl, url, lang).headOption.map(_.attr("href"))
  }

  def findPotentialNextPageLinks(htmlDoc: Document, baseUrl: String, url: String, lang: SupportedLanguages.Value): Seq[Element] = {
    val params = lang match {
      case SupportedLanguages.PL => LinkFindingParameters(new java.util.Locale("pl_PL"), langToKeyWords(lang))
      case SupportedLanguages.EN => LinkFindingParameters(Locale.ENGLISH, langToKeyWords(lang))
    }

    findLinksAlgorithm(htmlDoc, baseUrl, url, params)
  }

  private def attributeContain(attr: String, contain: String)(e: Element) = e.hasAttr(attr) && e.attr(attr).contains(contain)

  private def filterByAttributesThatContainAny(attr: String, values: Seq[String])(e: Element) = values.map(v => attributeContain(attr,v)(e)).exists(x=>x)

  private def filerByKeywords(params: LinkFindingParameters, elems: Seq[Element]): Seq[Element] = {
    elems.filter(element => {
      val text = element.text().toLowerCase(params.locale)
      params.keywords.exists(keyword => text.contains(keyword))
    })
  }

  /**
   *
   * Algorithm:
   *  Find all elements <a> which have attribute 'href' present.
   *  Filter them by 'href' that only points to same base url (removes links to other websites)
   *
   *  Then links are filtered using several techniques and results are merged together
   *
   *  1. Anchors which text contain some keyword. Keywords are specified for appropriate language
   *  2. Anchors which have attribute rel="next" - this is quite commonly used for pagination
   *  3. Anchors which have class attribute with specific css classes
   *
   *  Results are merged together to only unique results. Uniqueness is based on "href" attribute.
   *
   *  Unique results are sorted by longest prefix to given URL. for example
   *  if analysis for page http://www.somepage.com/products/ABC123 was requested, then link http://www.somepage.com/products/ABC123?page=2 has longer prefix than link
   *  http://www.somepage.com/contact-us
   *
   *  Relative urls are also supported.
   *
   *
   * @param htmlDoc - jsoup document
   * @param baseUrl - base url derived from requested URL
   * @param url - request URL for analysis
   * @param params - language specific parameters
   * @return
   */
  protected[crawler] def findLinksAlgorithm(htmlDoc: Document, baseUrl: String, url: String, params: LinkFindingParameters): Seq[Element] = {
    val allLinks = findAllLinks(htmlDoc).filter(filterToOnlyThoseWithBaseUrl(baseUrl))
    val withRelNext = findLinksWithRelNext(htmlDoc).filter(filterToOnlyThoseWithBaseUrl(baseUrl))
    val containingKeywords = filerByKeywords(params, allLinks)

    val attrHasKeywords = {for {
      attrToCheck <- attributesToCheckForKeywords
    } yield {
      allLinks.filter(filterByAttributesThatContainAny(attrToCheck, params.keywords))
    }}.flatten.toSeq

    val withCssClasses = allLinks.filter(filterByAttributesThatContainAny("class", cssClasses))

    val allCandidates = Seq(withRelNext, containingKeywords, attrHasKeywords, withCssClasses).flatten

    val uniqueCandidates = allCandidates.map(candidate => candidate.attr("href") -> candidate ).toMap.values.toSeq

    orderByLongestPrefix(baseUrl, url)(uniqueCandidates)
  }

//  implicit def toElements(buff: mutable.Buffer[Element]): Elements = new Elements(buff)
  //implicit def toElementsSeq(buff: Seq[Element]): Elements = new Elements(buff)
//  implicit def toElementsList(buff: List[Element]): Elements = new Elements(buff)

}

case class NextPageLinkRaw(url: String)
