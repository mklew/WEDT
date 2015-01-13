package wedt

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import wedt.crawler.WebsiteToXml._
import scala.collection.JavaConversions._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
class NextPageFinderSpec extends FlatSpec with Matchers with SampleData {

  import wedt.crawler.WebsiteToXml._
  import wedt.crawler.NextPageFinder._
  import wedt.crawler.UrlToBaseUrl._

  "NextPageFinder" should "find all links in page" in {
    val doc = toJsoupDoc(dummy.html, dummy.baseUrl).right.get

    val allLinks = findAllLinks(doc)

    allLinks should have size 7
  }

//  it should "filter links to only those pointing to same url" in {
//    fail()
//  }
//
//  it should "filter links to only those containing key words" in {
//    fail()
//  }
//
//  it should "find links with keywords in it" in {
//    fail()
//  }

  it should "order links by their longest prefix" in {
    val doc = toJsoupDoc(dummy.html, dummy.baseUrl).right.get

    val allLinks = findAllLinks(doc)
    val ordered = orderByLongestPrefix(dummy.baseUrl, dummy.url)(allLinks)

    ordered.head.attr("href") shouldBe "http://thaturl.pl/review/12345?page=2"
    ordered.tail.head.attr("href") shouldBe "http://thaturl.pl/12345"
  }

  it should "order links by their longest prefix in relative" in {
    val doc = toJsoupDoc(dummyRelative.html, dummyRelative.baseUrl).right.get

    val allLinks = findAllLinks(doc)
    val ordered = orderByLongestPrefix(dummyRelative.baseUrl, dummyRelative.url)(allLinks)

    ordered.head.attr("href") shouldBe "/review/12345?page=2"
    ordered.tail.head.attr("href") shouldBe "http://thaturl.pl/12345"
  }

  it should "find links with rel='next'" in {

    val doc = toJsoupDoc(dummy.html, dummy.baseUrl).right.get

    findLinksWithRelNext(doc) should have size 1
  }

  it should "filter links to only those that match base url" in {
    val doc = toJsoupDoc(dummy.html, dummy.baseUrl).right.get
    val allLinks = doc.select("a")
    allLinks.filter(filterToOnlyThoseWithBaseUrl(dummy.baseUrl)) should have size 6
  }

  it should "give base url for url" in {
    import wedt.crawler.UrlToBaseUrl._

    toBaseUrl(dummy.url) shouldBe "http://thaturl.pl/"
    toBaseUrl(amazon.url) shouldBe "http://www.amazon.co.uk/"
    toBaseUrl(opineo.url) shouldBe "http://www.opineo.pl/"
    toBaseUrl(ceneo.url) shouldBe "http://www.ceneo.pl/"
    toBaseUrl(gastronauci.url) shouldBe "http://www.gastronauci.pl/"
  }

}
