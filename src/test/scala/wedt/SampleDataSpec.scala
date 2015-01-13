package wedt

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
class SampleDataSpec extends FlatSpec with Matchers with SampleData {

  import wedt.crawler.WebsiteToXml._

  "Dummy sample"  should "be jsoup document" in {
    val doc: Document = Jsoup.parse(dummy.html, dummy.url)
  }

  "Amazon sample" should "be valid jsoup document" in {
    toJsoupDoc(amazon.html, amazon.url)
  }

  "Gastronauci sample" should "be valid jsoup document" in  {
    val doc: Document = toJsoupDoc(gastronauci.html, gastronauci.url).right.get

    val r = doc.select("a")

    r.size() should be > 0
  }

  "Ceneo sample" should "be valid jsoup document" in  {
    val doc: Document = toJsoupDoc(ceneo.html, ceneo.url).right.get
    val r = doc.select("a")

    r.size() should be > 0
  }

  "Opineo sample" should "be valid jsoup document" in  {
    val doc: Document = toJsoupDoc(opineo.html, opineo.url).right.get
    val r = doc.select("a")

    r.size() should be > 0
  }
}
