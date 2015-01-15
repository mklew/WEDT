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
    val r = SentimentAnalyzer.analyzePolish(review, stemmedWords, stemmer)

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


  "English analyzer" should "produce stems for english text" in {
    val review = "The first thing I will point out before reviewing the toothbrush, is that it comes with a 2 prong plug, " +
      "so my initial excitement at it arriving was somewhat dampened by the realisation that I needed an adapter before" +
      " I could charge it!\n\nI have had my toothbrush for about a week now, and I absolutely love it. I have used cheaper " +
      "electric tootchbrushes before, but I notice a huge difference with this one. I have suffered with sensitivity" +
      " in my teeth at times, so the idea of the 3 settings, including the sensitive option, really appealed to me. " +
      "However, I thought I would give the whitening option a go, and so far no sensitivity issues! I always notice " +
      "my teeth whiter and feeling cleaner with an electric toothbrush compared to a manual, however I definitely notice" +
      " an improvement in their colour with this new brush (and that is without a whitening tootpaste).\nMy teeth feel" +
      " and look clea, fresh and plaque free, between my teeth look cleaner, so the specially designed head seems to be " +
      "doing its job. Also, I don't like really soft toothbrushes, or round heads on an electric toothbrush, so I find this " +
      "one perfect. I haven't had any issues with it being too harsh, gum bleeding, and i also have a permanenet retainer " +
      "which it seems to be gentle enough on whilst easily cleaning around it.\n\nI haven't yet had to recharge my toothbrush, " +
      "which was a concern as I previously used a bettery powered brush.\n\nOther nice points with the brush are that it comes " +
      "with a spare head, and a nice little case so it can travel.\n\nI was never sure about spending larger amounts of money " +
      "on a toothbrush, but I would class this as mid range price-wise, there are more expensive brushes about, but for me this" +
      " brush is perfect and I would highly recommend it."


    val language = SupportedLanguages.EN

    val words = loadDictionaries(language)

    val stemmedWords = DictionaryStemmer.stemDictionary(words)
    val stemmer = Stemmers.getStemmer(language)
    val r = SentimentAnalyzer.analyzeEnglish(review, stemmedWords, stemmer)

    println(
      s"""
        | Positive words count: ${r.positiveWordsCount}
        | Negative words count: ${r.negativeWordsCount}
        | Ambiguous words count: ${r.ambiguous.size}
        | Neutral words count: ${r.neutral.size}
        | Total words count: ${r.totalWords}
        |
        | Positive words: ${r.positiveWords}
        | Negative words: ${r.negativeWords}
      """.stripMargin)
  }
}


