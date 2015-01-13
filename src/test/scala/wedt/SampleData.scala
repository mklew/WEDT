package wedt

import wedt.ws.RawWebsite

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
trait SampleData {

  def readFileAsString(path: String) = scala.io.Source.fromURL(getClass.getResource(path), "UTF-8").getLines().mkString

  lazy val amazon: RawWebsite = {
    val body = readFileAsString("/amazon.html")
    val url = "http://www.amazon.co.uk/product-reviews/B007WSAANA/"
    RawWebsite(url, body)
  }

  lazy val dummy: RawWebsite = {
    val body = readFileAsString("/syntetic.html")
    val url = "http://thaturl.pl/review/12345"
    RawWebsite(url, body)
  }

  lazy val dummyRelative: RawWebsite = {
    val body = readFileAsString("/syntetic-relative.html")
    val url = "http://thaturl.pl/review/12345"
    RawWebsite(url, body)
  }

  lazy val ceneo: RawWebsite = {
    val body = readFileAsString("/ceneo.html")
    val url = "http://www.ceneo.pl/17664350"
    RawWebsite(url, body)
  }

  lazy val opineo: RawWebsite = {
    val opineo = readFileAsString("/opineo.html")
    val url = "http://www.opineo.pl/opinie/inexus-pl"
    RawWebsite(url, opineo)
  }

  lazy val gastronauci: RawWebsite = {
    val body = readFileAsString("/gastronauci.html")
    val url = "http://www.gastronauci.pl/pl/303-restauracja-i-winiarnia-borpince-warszawa-srodmiescie"
    RawWebsite(url, body)
  }

  lazy val cokupic: RawWebsite = {
    val body = readFileAsString("/cokupic.html")
    val url = "http://cokupic.pl/produkt/Lark-FreeBird-35AT-35-LarkMap-Polska"
    RawWebsite(url, body)
  }

  lazy val dummyDisjointHtml = readFileAsString("/disjoint-elements.html")

}
