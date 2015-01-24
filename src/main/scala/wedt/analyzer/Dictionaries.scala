package wedt.analyzer

import java.io.StringReader

import morfologik.stemming.PolishStemmer
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.getopt.stempel
import wedt.crawler.SupportedLanguages

import scala.collection.JavaConverters._

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

  lazy val polishDictionaries = DictionaryStemmer.stemDictionary(loadDictionaries(SupportedLanguages.PL))

  lazy val englishDictionaries = DictionaryStemmer.stemDictionary(loadDictionaries(SupportedLanguages.EN))

  def getDictionary(language: SupportedLanguages.Value) = language match {
    case SupportedLanguages.PL => polishDictionaries
    case SupportedLanguages.EN => englishDictionaries
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
      case SupportedLanguages.PL => new MixedStemmer
      case SupportedLanguages.EN => new EnglishStemmer
    }
  }
}

case class Words(positive: Set[String], negative: Set[String], lang: SupportedLanguages.Value)

case class StemmedWords(positive: Seq[String], negative: Seq[String], lang: SupportedLanguages.Value)

trait Stemmer {
  def stem(w: String): Seq[String]
}

class MixedStemmer extends Stemmer {
  val mainStemmer = new MorfologikStemmer
  val auxiliaryStemmer = new stempel.Stemmer()

  override def stem(word: String): Seq[String] = {
    val stems = mainStemmer.stem(word)
    val auxiliaryStem = auxiliaryStemmer.stem(word, true)
    if(stems.exists(s => s == auxiliaryStem) || stems.isEmpty)
      Seq(auxiliaryStem)
    else
      stems
  }

}

class MorfologikStemmer extends Stemmer {
  
  val stemmer = new PolishStemmer

  override def stem(word: String): Seq[String] = {
    val wordData = asScalaBufferConverter(stemmer.lookup(word)).asScala
    wordData.map(wd => wd.getStem.toString)
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
