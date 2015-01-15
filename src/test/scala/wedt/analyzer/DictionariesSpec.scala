package wedt.analyzer

import morfologik.stemming.PolishStemmer
import org.scalatest.{Matchers, FlatSpec}
import wedt.crawler.SupportedLanguages
import Dictionaries._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 15/01/15
 */
class DictionariesSpec extends FlatSpec with Matchers {

  "Dictionaries" should "load correct dictionary" in {
    val language = SupportedLanguages.PL

    val words = loadDictionaries(language)

    println(
      s"""
        | Positive words size ${words.positive.size}
        | Negative words size ${words.negative.size}
      """.stripMargin)

    println(words.positive.take(100))

    words.positive.size should be > 0
    words.negative.size should be > 0
  }

//  it should "create stems of words from dictionaries" in {
//
//  }

  it should "create stems from dictionaries" in {
    val language = SupportedLanguages.PL

    val words = loadDictionaries(language)


    val stemmedWords = DictionaryStemmer.stemDictionary(words)
    println(
      s"""
        | Positive stems size ${stemmedWords.positive.size}
        | Negative stems  size ${stemmedWords.negative.size}
      """.stripMargin)

    println(stemmedWords.positive.toSeq.sortBy(_.size).take(100))
    println(stemmedWords.negative.take(100))

    stemmedWords.positive.size should be > 0
    stemmedWords.negative.size should be > 0
  }

  it should "stem" in {
    val stemmer = new PolishStemmer()

    var word = "znakomity"

    val results = stemmer.lookup(word)
    val it = results.iterator()
    println("wyniki")
    while(it.hasNext) {
      val n = it.next
      println("stem: " + n.getStem.toString)

      println("tag: " + n.getTag.toString)
      println(n)
    }



  }
}

class SentimentAnalyzerSpec extends FlatSpec with Matchers {

  "SentimentAnalzer" should "analyze review" in {
    val review = "Węgierska restauracja w piwnicy przy Zgodzie. Jak to w piwnicy, bezpretensjonalnie i przytulnie," +
      " miejsce ma swój klimat. Nas ujęła przede wszystkim obsługa – kelner, który wszystko załatwiał " +
      "szybko i sprawnie, był bezpośredni, ale taktowny, i zdekantował nam wino, co niestety ciągle jest " +
      "wyjątkiem od reguły w warszawskiej gastronomii. Zresztą wino było świetne (Cabernet Franc,  Villany). " +
      "Warto też wspomnieć, że wybór w karcie win węgierskich przedni, widać, że obszar bardzo dopieszczony przez właściciela. " +
      "Poza tym na parterze jest też mały, ale dobrze wyposażony sklepik z węgierskimi winami – polecam.W końcu jedzenie - bardzo dobre. " +
      "Zupa gulaszowa z wołowiny bardzo smaczna, choć trochę zbyt łagodna, ale można samemu sobie doprawić suszoną papryczką. " +
      "Gulasz wieprzowy również bez zarzutu. Cielęcina smażona z cebulą i ziemniakami smaczna, polędwica wołowa z wątróbką gęsią i camembertem " +
      "(zbytnio wysuszonym) oraz pastą z orzecha włoskiego i talarkami ziemniaczanymi - aż 94 złote za te przyjemność - " +
      "jedynie poprawna, niewarta swojej ceny. Tatar wołowy mnie również nie przekonał, wydaje mi się, że mielony nie siekany," +
      " przyprawiony pieprzem i przede wszystkim papryką – dla koneserów papryki. Zresztą cebulkę i ogórka do tatara " +
      "sam musiałem sobie posiekać... Sałatka z grillowanym serem zaskakująco smaczna. Summa summarum – wyszliśmy wszyscy" +
      " z pełnymi brzuchami i bardzo zadowoleni z kolacji. Polecam miejsce zarówno na randkę (choć należy pamiętać, że " +
      "jedzenie jest ciężkie) jak i spotkanie z rodziną czy przyjaciółmi."
    val language = SupportedLanguages.PL

    val words = loadDictionaries(language)


    val stemmedWords = DictionaryStemmer.stemDictionary(words)
    val stemmer = Stemmers.getStemmer(language)
    val r = SentimentAnalyzer.analyze(review, stemmedWords, stemmer)

    println(
      s"""
        | Positive words count: ${r.positiveWordsCount}
        | Negative words count: ${r.negativeWordsCount}
        | Total words count: ${r.totalWords}
        |
        | Positive words: ${r.positiveWords}
        | Negative words: ${r.negativeWords}
      """.stripMargin)
  }
}


