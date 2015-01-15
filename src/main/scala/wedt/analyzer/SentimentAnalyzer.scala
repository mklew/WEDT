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

  def analyzePolish(review: String, dictionaries: StemmedWords, stemmer: Stemmer): AnalyzedReview = {

    val words = review.split(" ").map(_.toLowerCase(SupportedLanguages.getLocale(dictionaries.lang)))

    val stemmedReview = DictionaryStemmer.stemsForWords(words, stemmer)

    val total = words.size

    doAnalyze(review, total, dictionaries, stemmedReview)
  }

  def analyzeEnglish(review: String, dictionaries: StemmedWords, stemmer: Stemmer): AnalyzedReview = {
    val stemmedReview = stemmer.stem(review)

    val total = review.split(" ").size

    doAnalyze(review, total, dictionaries, stemmedReview)
  }

}

case class AnalyzedReview(review: String, positiveWords: Seq[String], negativeWords: Seq[String], ambiguous: Seq[String], neutral: Seq[String], totalWords: Int, stems: Seq[String]) {
  lazy val positiveWordsCount = positiveWords.size
  lazy val negativeWordsCount = negativeWords.size
}


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