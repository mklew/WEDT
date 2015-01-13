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

  "Dummy sample" should "be valid xml" in {
    toXml(dummy.html) shouldBe 'right
  }

  it should "be jsoup document" in {
    val doc: Document = Jsoup.parse(dummy.html, dummy.url)
  }

  "Amazon sample" should "be valid jsoup document" in {
    val doc: Document = Jsoup.parse(amazon.html, amazon.url)
  }

  "Gastronauci sample" should "be valid jsoup document" in  {
    val doc: Document = Jsoup.parse(gastronauci.html, gastronauci.url)

    val r = doc.select("a")

    r.size() should be > 0
  }

  "Ceneo sample" should "be valid jsoup document" in  {
    val doc: Document = Jsoup.parse(ceneo.html, ceneo.url)
    val r = doc.select("a")

    r.size() should be > 0
  }

  "Opineo sample" should "be valid jsoup document" in  {
    val doc: Document = Jsoup.parse(opineo.html, opineo.url)
    val r = doc.select("a")

    r.size() should be > 0
  }
}
