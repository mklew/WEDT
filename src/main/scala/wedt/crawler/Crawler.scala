package wedt.crawler

import com.typesafe.scalalogging.slf4j.StrictLogging

import scala.util.{Failure, Success, Try}
import scala.xml._

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

  def toXml(raw: String): Either[String, scala.xml.Elem] = {
    Try(scala.xml.XML.loadString(raw)) match {
      case Success(xml) => Right(xml)
      case Failure(err) => {
        logger.error("Failed to transform website to XML", err)
        Left(err.getMessage)
      }
    }
  }
}

object NextPageFinder {

  /**
   * Unfortunately html is rarely valid xml so we will be working with raw strings
   * @param html
   */
  def findAllLinks(html: String) = {



  }


}

case class NextPageLinkRaw(raw: String)
