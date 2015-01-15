package wedt.analyzer

import java.io.StringReader

import morfologik.stemming.{IStemmer, PolishStemmer}
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import wedt.crawler.SupportedLanguages

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 15/01/15
 */
object Dictionaries {
  def loadDictionaries(language: SupportedLanguages.Value): Words = {
    val positiveWords = scala.io.Source.fromURL(getClass.getResource(s"/dict-positive-${language.toString}"), "UTF-8").getLines().toSet
    val negativeWords = scala.io.Source.fromURL(getClass.getResource(s"/dict-negative-${language.toString}"), "UTF-8").getLines().toSet
    //val stopWords = scala.io.Source.fromURL(getClass.getResource(s"/stop-words-${language.toString}"), "UTF-8").getLines().toSet

    Words(positiveWords, negativeWords, language)
  }
}

object DictionaryStemmer {

  protected[analyzer] def stemsForWords(words: Seq[String], stemmer: Stemmer): Seq[String] = {
    val stems = for {
      word <- words
    } yield {
      stemmer.stem(word)
    }

    stems.flatten
  }

  def stemDictionary(words: Words): StemmedWords = {
    val stemmer = Stemmers.getStemmer(words.lang)

    StemmedWords(stemsForWords(words.positive.toSeq, stemmer), stemsForWords(words.negative.toSeq, stemmer),
       words.lang)
  }
}

object Stemmers {
  
  def getStemmer(lang: SupportedLanguages.Value): Stemmer = {
    lang match {
      case SupportedLanguages.PL => new MorfologikStemmer
      case SupportedLanguages.EN => new EnglishStemmer
    }
  }
}

case class Words(positive: Set[String], negative: Set[String], lang: SupportedLanguages.Value)

case class StemmedWords(positive: Seq[String], negative: Seq[String], lang: SupportedLanguages.Value)

trait Stemmer {
  def stem(w: String): Seq[String]
}

class MorfologikStemmer extends Stemmer {
  
  val stemmer = new PolishStemmer
  
  override def stem(word: String): Seq[String] = {
    val wordData = stemmer.lookup(word)
    val it = wordData.iterator()
    val stemsBuffer: scala.collection.mutable.Buffer[String] = scala.collection.mutable.ArrayBuffer()
    while(it.hasNext) {
      val n = it.next()

      val stem = n.getStem.toString
      stemsBuffer.append(stem)
    }
    stemsBuffer.toSeq
  }
}

class EnglishStemmer extends Stemmer {
  override def stem(w: String): Seq[String] = {
    val englishAnalyzer = TextAnalyzer.englishAnalyzer
    TextAnalyzer.getTokens(w, englishAnalyzer)
  }
}

object TextAnalyzer {

  def englishAnalyzer = new EnglishAnalyzer

  //  def apply(stopWords: Set[String]): TextAnalyzer = {
  //
  //    val stopwordsSet = new CharArraySet(stopWords, true)
  //    new TextAnalyzer(stopwordsSet)
  //  }

  def getTokens(textToAnalyze: String, analyzer: Analyzer): Seq[String] = {
    val stream = analyzer.tokenStream("field", new StringReader(textToAnalyze))

    val termAtt= stream.addAttribute(classOf[CharTermAttribute])

    val stemmedResults: scala.collection.mutable.Buffer[String] = scala.collection.mutable.ArrayBuffer()
    try {
      stream.reset()
      while (stream.incrementToken) {
        stemmedResults.append(termAtt.toString)
      }
      stream.end()
    } finally {
      stream.close()
      analyzer.close()
    }
    stemmedResults.toSeq
  }
}
