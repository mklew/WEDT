package wedt.crawler

import java.net.URL

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.jsoup.Jsoup
import org.jsoup.select.Elements

import scala.collection.mutable
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

object NextPageFinder {

  val langToKeyWords = Map(
    SupportedLanguages.PL -> List("następna", "nastepny", "nastepna"),
    SupportedLanguages.EN -> List("next")
  )

  val cssClasses = List("next", "next_page")

  /**
   * Unfortunately html is rarely valid xml so we will be working with raw strings
   * @param htmlDoc - jsoup document
   */
  def findAllLinks(htmlDoc: Document): Seq[Element] = {
    htmlDoc.select("a").filter(_.hasAttr("href"))
  }

  def findLinksWithRelNext(htmlDoc: Document): Elements = {
    htmlDoc.select("""a[rel="next"]""")
  }

  def findLinksWhichMatchAttributesAndValues(htmlDoc: Document, attr: String, value: String): Elements = {
    htmlDoc.select(s"""a[$attr~="$value"]""")
  }

  private def isRelative(href: String) = href.startsWith("/")

  def filterToOnlyThoseWithBaseUrl(baseUrl: String)(element: Element): Boolean = {
    val href = element.attr("href")
    isRelative(href) || href.startsWith(baseUrl)
  }

  def joinWithRelative(baseUrl: String, relative: String): String = {
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

  def longestPrefix(baseUrl: String, url: String)(element: Element): String = {
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

  def orderByLongestPrefix(baseUrl: String, url: String)(elements: Seq[Element]): Seq[Element] = {
    elements.map(e => e -> longestPrefix(baseUrl, url)(e)).toSeq.sortBy(_._2.length).reverse.map(_._1)
  }

  def findLinksAlgorithm(htmlDoc: Document, baseUrl: String, keywords: List[String]): Seq[Element] = {
    /*
    Link finding:
    1. try with rel next
    2. fallback to all links filtered by keywords in their text
     */

    val allLinks = htmlDoc.select("a")
    val withRelNext = findLinksWithRelNext(htmlDoc)

    val linksForFurtherFiltering = if(withRelNext.nonEmpty) {
      withRelNext
    }
    else {
      allLinks
    }

    linksForFurtherFiltering.filter(filterToOnlyThoseWithBaseUrl(baseUrl))
  }

  implicit def toElements(buff: mutable.Buffer[Element]): Elements = new Elements(buff)
  //implicit def toElementsSeq(buff: Seq[Element]): Elements = new Elements(buff)
  implicit def toElementsList(buff: List[Element]): Elements = new Elements(buff)

}

case class NextPageLinkRaw(url: String)
