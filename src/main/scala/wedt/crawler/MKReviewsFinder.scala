package wedt.crawler

import java.util.Locale

import com.typesafe.scalalogging.slf4j.StrictLogging
import org.jsoup.nodes.{Document, Element}

import scala.collection.JavaConversions._

/**
 * Created by gospo on 20.01.15.
 */
class MKReviewsFinder(val parentsDepth: Int) extends ReviewsFinder with StrictLogging {

  def parentsUntilBody(e: Element, depth: Int): Seq[Element] = {
    var startFrom = e;
    (0 until depth).foreach(x=> startFrom=e.parent())
    startFrom.parents().takeWhile(_.tagName() != "body")
  }

  protected[crawler] def toElementWithParents(e: Element): ElementWithParents = ElementWithParents(e, parentsUntilBody(e, parentsDepth))

  case class ElementWithParents(e: Element, parents: Seq[Element])



  protected[crawler] def findBody(htmlDoc: Document): Element = htmlDoc.select("body").first()


  def buildElementsSelectionQuery(searchAttrs: Seq[String], searchValues: Seq[String]): String =
    (for(attr <- searchAttrs; value <- searchValues) yield s"[$attr~=$value]").mkString(",")

  val searchAttrs = Seq("class", "id")
  val searchValues = Seq("review", "opinion", "comment", "author", "rating", "komentarz", "ocena", "autor", "opinia", "recenzja")
  protected[crawler] def findReviewsElements(htmlDoc: Document): Seq[ElementWithParents] = {
    val body = findBody(htmlDoc)
    val elementsThatContainSomeAttributes = body.select(buildElementsSelectionQuery(searchAttrs, searchValues))
    logger.info(s"Found ${elementsThatContainSomeAttributes.size} elements that contain keywords")
    elementsThatContainSomeAttributes.map(toElementWithParents)
  }

  protected[crawler] def groupElements(disjointElements: Seq[ElementWithParents]) = disjointElements.groupBy(_.parents).values.toSeq


  def stdCondition(group: Seq[String]): Boolean = {
    val sum = group.map(e => e.length).sum
    val mean = sum / group.length
    val std = Math.sqrt(group.map(e => Math.pow(e.length - mean, 2)).sum / group.length)

    std >= 3
  }

  val contextAttrs = searchAttrs
  val contextValues = Seq("toolbar", "usefulness" ,"header", "date", "nick", "name", "author", "score", "rate", "rating", "data", "autor", "ocena")
  def cutOutContextualElements(group: Seq[Element]): Seq[Element] = {
    group.foreach(e => e.select(buildElementsSelectionQuery(contextAttrs, contextValues)).foreach(e=>e.remove()))
    group
  }

  def cutOutContextualData(group: Seq[String]): Seq[String] = {
    cutOutContextualSuffix(cutOutContextualPrefix(group))
  }

  def cutOutContextualSuffix(group: Seq[String]): Seq[String] = {
    def cut(t: String, off:  Int) = t.substring(t.length - off)
    def map(t: String, contextual:  String) = if(t.endsWith(contextual)) t.substring(0, t.length - contextual.length) else t
    cutOutContextual(cut, map)(group)
  }

  def cutOutContextualPrefix(group: Seq[String]): Seq[String] = {
    def cut(t: String, off:  Int) = t.substring(0, off)
    def map(t: String, contextual:  String) = if(t.startsWith(contextual)) t.substring(contextual.length) else t
    cutOutContextual(cut, map)(group)
  }

  def cutOutContextual(cut: (String, Int) => String, map: (String, String) => String)(group: Seq[String]): Seq[String] = {
    val minCount = 0.5 * group.length
    val minLen = 3
    var off = minLen

    def groupBy(t: String): String = if(off > t.length) t else cut(t, off)
    def groupWithContext(off: Int): Seq[String] = group.groupBy(groupBy).filter(_._1.length >= off).foldLeft(Seq[String]())((m,x) => if(m.length > x._2.length) m else x._2)
    while(groupWithContext(off).length >= minCount) off = off + 1
    off = off - 1

    if(off >= minLen) {
      val contextual = cut(groupWithContext(off)(0), off)
      group.map(t => map(t, contextual))
    }
    else group
  }

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
  def findReviews(htmlDoc: Document, lang: SupportedLanguages.Lang, params: ReviewParams): Reviews = {
    logger.info(s"findReviews called. Params: $params")
    val reviewsElements = findReviewsElements(htmlDoc)
    logger.info(s"Got ${reviewsElements.size} disjoint elements")
    val reviewsGroups = groupElements(reviewsElements)
      .map(g => g.map(_.e))
      .map(g => g.groupBy(e=>e.tag()).maxBy(_._2.size)._2) // select the most numerous tag in each group
      .filter(g => g.length >= params.minimumReviews) // comments count constraint
      .map(g => cutOutContextualElements(g)) // cut out contextual elements
      .map(g => g.map(_.text()))
      .map(g => cutOutContextualData(g)) // cut out contextual elements
      .map(g => g.filter(t => t.length >= 5)) // comments length constraint
      .filter(g => stdCondition(g)) // standard deviation bound - not fully utilized - requires some research to find out useful value
      .maxBy(g => g.size) // select the largest group

    reviewsGroups.map(text=>Review(text))
  }
}