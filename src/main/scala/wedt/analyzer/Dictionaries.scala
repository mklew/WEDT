package wedt.analyzer

import morfologik.stemming.{IStemmer, PolishStemmer}
import wedt.crawler.SupportedLanguages

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 15/01/15
 */
object Dictionaries {
  def loadDictionaries(language: SupportedLanguages.Value): Words = {
    val positiveWords = scala.io.Source.fromURL(getClass.getResource(s"/dict-positive-${language.toString}"), "UTF-8").getLines().toSet
    val negativeWords = scala.io.Source.fromURL(getClass.getResource(s"/dict-negative-${language.toString}"), "UTF-8").getLines().toSet

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
      case SupportedLanguages.EN => ??? // TODO add Porter or Snowball stemmer from Lucence 
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
