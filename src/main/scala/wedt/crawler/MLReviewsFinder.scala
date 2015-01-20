package wedt.crawler

import java.util.Locale

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.jsoup.nodes.{Document, Element}

import scala.collection.JavaConversions._

object MLReviewsFinder extends ReviewsFinder with StrictLogging {

  // TODO more keywords
  val anyAttributeValues = Seq("review", "opinion", "comment", "author", "rating", "komentarz", "ocena", "autor", "opinia", "recenzja")

  protected[crawler] def findAll(root: Element, predicate: Element => Boolean): Seq[Element] = {
    val results = for {
      child <- root.children().toIndexedSeq
    } yield {
      if(predicate(child)) Seq(child) else findAll(child, predicate)
    }
    results.flatten.toSeq
  }

  def anyAttributeContainAnyValue(values: Seq[String])(e: Element): Boolean = {
    e.attributes().exists(a => {
      val attrVal = a.getValue.toLowerCase(Locale.ENGLISH)
      values.exists(contain => attrVal.contains(contain))
    })
  }

  def parentsUntilBody(e: Element): Seq[Element] = {
    e.parents().takeWhile(_.tagName() != "body")
  }

  protected[crawler] def toElementWithParents(e: Element): ElementWithParents = ElementWithParents(e, parentsUntilBody(e))

  case class ElementWithParents(e: Element, parents: Seq[Element])

  protected[crawler] def onlyDisjointElements(seq: Seq[ElementWithParents]): Seq[ElementWithParents] = {

    def addToDisjoint(disjoint: Seq[ElementWithParents], rest: Seq[ElementWithParents]): Seq[ElementWithParents] = {
      rest headOption match {
        case Some(e) => if(disjoint.exists(dis => elementContainOther(dis, e))) addToDisjoint(disjoint, rest.tail) else addToDisjoint(Seq(e) ++ disjoint, rest.tail)
        case None => disjoint
      }
    }

    val sortedByNumberOfParents = seq.sortBy(_.parents.size)

    if(sortedByNumberOfParents.nonEmpty) {
      val first = sortedByNumberOfParents.head
      val minimumNumberOfParents = first.parents.size

      val disjointElements = seq.takeWhile(_.parents.size == minimumNumberOfParents)
      val restOfElements = seq.dropWhile(_.parents.size == minimumNumberOfParents)
      addToDisjoint(disjointElements, restOfElements)
    }
    else Seq()
  }

  def elementContainOther(ep: ElementWithParents, other: ElementWithParents): Boolean = {
    other.parents.contains(ep.e)
  }

  protected[crawler] def findBody(htmlDoc: Document): Element = htmlDoc.select("body").first()

  protected[crawler] def findAllWithKeyWords(root: Element) = findAll(root, anyAttributeContainAnyValue(anyAttributeValues))

  protected[crawler] def findDisjointElements(htmlDoc: Document) = {
    val body = findBody(htmlDoc)
    val elementsThatContainSomeAttributes = findAllWithKeyWords(body)
    logger.info(s"Found ${elementsThatContainSomeAttributes.size} elements that contain keywords")
    val withParents = elementsThatContainSomeAttributes.map(toElementWithParents)
    onlyDisjointElements(withParents)
  }

  protected[crawler] def groupDisjoint(disjointElements: Seq[ElementWithParents]) = disjointElements.groupBy(_.parents).values.toSeq


  /**
   * Algorithm.
   *    1. Find elements that have some attributes which contain values like "review", "opinion", "comment", "author", "rating"
   *    2. For each element, generate list of parents up to BODY
   *    3.
   *
   *
   * @param htmlDoc
   * @param lang
   * @param params
   * @return
   */
  override def findReviews(htmlDoc: Document, lang: SupportedLanguages.Lang, params: ReviewParams): Reviews = {
    logger.info(s"findReviews called. Params: $params")
    val disjointElements = findDisjointElements(htmlDoc)
    logger.info(s"Got ${disjointElements.size} disjoint elements")
    val reviewsGroups = groupDisjoint(disjointElements).map(s => s.map(_.e))

    logger.info(s"Grouped them into ${reviewsGroups.size} groups")

    val groupsWithComments = for {
      reviewGroup <- reviewsGroups
    } yield {
      val groupWithComments = reviewGroup.map(elementInGroup => {
        val allParagraphs = elementInGroup.select("p").toIndexedSeq
        val onlyLowestLevelDivs = elementInGroup.select("div").filter(div => {
          !div.children().exists(_.tagName() == "div")
        }).toSeq

        val allPotentialCommentNodes = allParagraphs ++ onlyLowestLevelDivs

        val comments = allPotentialCommentNodes.map(_.text()).filter(text => text.split(" ").size >= params.minimumWordsInReview)
        comments
      })

      val commentsInThatGroup = groupWithComments.flatten

      commentsInThatGroup
    }

    logger.info(s"Groups that contain some reviews: ${groupsWithComments.size}")
    logger.debug(groupsWithComments.mkString("\n\n\n"))

    val onlyGroupsWithMinimumOfReviews = groupsWithComments.filter(_.size >= params.minimumReviews)

    val groupsWithReviews: Seq[Seq[String]] = if(groupsWithComments.nonEmpty && onlyGroupsWithMinimumOfReviews.isEmpty) {
      val merged = groupsWithComments.flatten

      if(merged.size >= params.minimumReviews)
        Seq(merged)
      else Seq()
    } else onlyGroupsWithMinimumOfReviews

    logger.info(s"Groups that satify minimum reviews requirements: ${onlyGroupsWithMinimumOfReviews.size}")

    // TODO można albo wszystkie grupy złączyć, albo wziąć tą z największą ilością komentarzy. Chyba lepiej jak się weźmie tą z najwyższą ilościa komentarzy, to będzie większy precision
    val finalReviews = if(groupsWithReviews.nonEmpty) groupsWithReviews.maxBy(_.size) else Seq()

    logger.info(s"Final reviews number: ${finalReviews.size}")
    finalReviews.headOption.map(r => {
      logger.info(s"First review: $r")
    })


    finalReviews.map(Review)
  }

}

