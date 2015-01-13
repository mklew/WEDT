package wedt.crawler

import org.scalatest.{FlatSpec, Matchers}
import wedt.SampleData
import wedt.crawler.WebsiteToXml._
import scala.collection.JavaConversions._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
class ReviewsFinderSpec extends FlatSpec with Matchers with SampleData {

  import wedt.crawler.ReviewsFinder._

  "ReviewsFinder" should "find all elements with specific keywords merging them to only disjoint elements" in {
    val doc = toJsoupDoc(dummyDisjointHtml).right.get
    val disjointElements = findDisjointElements(doc)
    disjointElements should have size 5

    for {
      e <- disjointElements
    } {
      e.e.hasAttr("expected-disjoint-element") shouldBe true
    }
  }

  it should "group disjoint elements by their common parent" in {
    val doc = toJsoupDoc(dummyDisjointHtml).right.get
    val disjointElements = findDisjointElements(doc)
    disjointElements should have size 5


    val reviewsGroups = groupDisjoint(disjointElements)

    reviewsGroups should have size 2
  }

  def checkReviews(name: String, html: String, baseUrl: String)(expectedReviewsStartWith: List[String]) = {
    val doc = toJsoupDoc(html, baseUrl).right.get
    val reviews = findReviews(doc, SupportedLanguages.PL, ReviewParams(minimumReviews = 3, minimumWordsInReview = 10))
    for {
      reviewStartsWith <- expectedReviewsStartWith
    } {
      it should s"find review in $name starting with: $reviewStartsWith" in {
        reviews.exists(c => c.review.startsWith(reviewStartsWith)) shouldBe true
      }
    }
  }

  checkReviews("gastronauci", gastronauci.html, gastronauci.baseUrl) {
    List("Miło, sympatyczna obsługa, dobre jedzenie",
      "Węgierska restauracja w piwnicy przy Zgodzie. Jak to w piwnicy, bezpretensjonalnie ",
      "Wybrałem się tam z żoną, aby przypomnieć sobie nasze "
    )
  }

  checkReviews("cokupic", cokupic.html, cokupic.baseUrl) {
    List(
      "wszystko super, przesyłka szybka oraz najtaniej. Polecam wszystkim. Produkt dokładnie taki jak ",
      "Pomimo kilku wad polecam. Nawigacja się sprawdziła. Stosunek cena-jakość 10/10"
    )
  }

}
