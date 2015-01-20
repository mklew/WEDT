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
    val minFreq = 0.3;
    val minLen = 3

    var off = 3
    var continue = true
    def groupBySuffix(t: String): String = if(off > t.length) t else t.substring(t.length - off)
    do {
      val groupsBySuffix = group.groupBy(groupBySuffix).filter(_._1.length >= off)
      if(groupsBySuffix.isEmpty)
        continue = false
      else {
        val groupWithContext = groupsBySuffix.maxBy(_._2.length)._2
        continue = groupWithContext.length >= minFreq * group.length
        off = off + 1
      }
    } while(continue)
    off = off - 2

    if(off > minLen) {
      val groupWithContext = group.groupBy(groupBySuffix).filter(_._1.length >= off).maxBy(_._2.length)._2
      val suffix = groupWithContext.head.substring(groupWithContext.head.length - off)
      group.map(t => if(t.endsWith(suffix)) t.substring(0, t.length - suffix.length) else t)
    }
    else group
  }

  def cutOutContextualPrefix(group: Seq[String]): Seq[String] = {
    val minFreq = 0.3
    val minLen = 3

    var off = 3
    var continue = true
    def groupByPrefix(t: String): String = if(off > t.length) t else t.substring(0, off)
    do {
      val groupsByPrefix = group.groupBy(groupByPrefix).filter(_._1.length >= off)
      if(groupsByPrefix.isEmpty)
        continue = false
      else {
        val groupWithContext = groupsByPrefix.maxBy(_._2.length)._2
        continue = groupWithContext.length >= minFreq * group.length
        off = off + 1
      }
    } while(continue)
    off = off - 2

    if(off > minLen) {
      val groupWithContext = group.groupBy(groupByPrefix).filter(_._1.length >= off).maxBy(_._2.length)._2
      val prefix = groupWithContext(0).substring(0, off)
      group.map(t => if(t.startsWith(prefix)) t.substring(prefix.length) else t)
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