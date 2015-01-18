package wedt.analyzer

import java.io.{StringReader, Reader}

import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.util.{CharArraySet, StopwordAnalyzerBase}
import org.apache.lucene.analysis.{Tokenizer, Analyzer}
import org.apache.lucene.analysis.en.{EnglishAnalyzer, PorterStemFilter}
import wedt.crawler.SupportedLanguages
import scala.collection.JavaConversions._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object SentimentAnalyzer {

  private def doAnalyze(review: String, total: Int, dictionaries: StemmedWords, stemmedReview: scala.Seq[String]) = {
    val checkedWithDictionaries = stemmedReview.map(stem => (stem, dictionaries.positive.contains(stem), dictionaries.negative.contains(stem)))

    val positiveUnique = checkedWithDictionaries.filter(x => x._2 && !x._3).map(_._1)
    val negativeUnique = checkedWithDictionaries.filter(x => !x._2 && x._3).map(_._1)
    val positiveAndNegative = checkedWithDictionaries.filter(x => x._2 && x._3).map(_._1)
    val neutral = checkedWithDictionaries.filter(x => !x._2 && !x._3).map(_._1)

    AnalyzedReview(review = review,
      positiveWords = positiveUnique,
      negativeWords = negativeUnique,
      ambiguous = positiveAndNegative,
      neutral = neutral,
      totalWords = total,
      stems = stemmedReview)
  }

  def analyzer(dictionaries: StemmedWords, stemmer: Stemmer, lang: SupportedLanguages.Value) = {
    lang match {
      case SupportedLanguages.PL => (r: String) => analyzePolish(r, dictionaries, stemmer)
      case SupportedLanguages.EN => (r: String) => analyzeEnglish(r, dictionaries, stemmer)
    }
  }

  protected[analyzer] def analyzePolish(review: String, dictionaries: StemmedWords, stemmer: Stemmer): AnalyzedReview = {

    val words = review.split(" ").map(_.toLowerCase(SupportedLanguages.getLocale(dictionaries.lang)))

    val stemmedReview = DictionaryStemmer.stemsForWords(words, stemmer)

    val total = words.size

    doAnalyze(review, total, dictionaries, stemmedReview)
  }

  protected[analyzer] def analyzeEnglish(review: String, dictionaries: StemmedWords, stemmer: Stemmer): AnalyzedReview = {
    val stemmedReview = stemmer.stem(review)

    val total = review.split(" ").size

    doAnalyze(review, total, dictionaries, stemmedReview)
  }

  def calculateSentiment(ar: AnalyzedReviewSimple): Double = {
    val denominator = (ar.neutral + ar.positive + ar.negative).toDouble

    val nominator = (ar.positive - ar.negative).toDouble

    nominator / denominator
  }

  def relevance(ar: AnalyzedReviewSimple, total: TotalWordsAcrossReviews): Double = {
    ar.totalWords.toDouble / total.total.toDouble
  }

  def totalWords(list: List[AnalyzedReviewSimple]): TotalWordsAcrossReviews = {
    TotalWordsAcrossReviews(list.map(_.totalWords).reduce(_ + _))
  }

  def overallSentiment(list: List[Post]): Double = {
    if(list.isEmpty) 0.0
    else {
      val totalSentiment = list.map(_.sentiment).reduce(_ + _)
      totalSentiment / list.size
    }
  }

}

case class Post(text: String, sentiment: Double, relevance: Double)

case class TotalWordsAcrossReviews(total: Int)

case class AnalyzedReview(review: String, positiveWords: Seq[String], negativeWords: Seq[String], ambiguous: Seq[String], neutral: Seq[String], totalWords: Int, stems: Seq[String]) {
  lazy val positiveWordsCount = positiveWords.size
  lazy val negativeWordsCount = negativeWords.size

  def toSimpleForm = AnalyzedReviewSimple(review, positiveWordsCount, negativeWordsCount, ambiguous.size, neutral.size, totalWords)
}

case class AnalyzedReviewSimple(review: String, positive: Int, negative: Int, ambiguous: Int, neutral: Int, totalWords: Int)

case class AnalyzedReviewWithSentiment(sentiment: Double, analyzed: AnalyzedReview)


//class MyAnalyzer extends Analyzer {
//  override def createComponents(fieldName: String, reader: Reader): TokenStreamComponents = {
//
//  val source: Tokenizer = new LowerCaseTokenizer(reader);
//  new TokenStreamComponents(source, new PorterStemFilter(source))
//}
//}


//
//class TextAnalyzer(stopWords: CharArraySet) extends EnglishAnalyzer {
//  override def createComponents(fieldName: String, reader: Reader): TokenStreamComponents = {
//    val source: Tokenizer = new LowerCaseTokenizer(reader)
//    new TokenStreamComponents(source, new PorterStemFilter(source))
//  }
//}