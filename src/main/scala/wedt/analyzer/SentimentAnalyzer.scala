package wedt.analyzer

import wedt.crawler.SupportedLanguages

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object SentimentAnalyzer {

  def analyze(review: String, dictionaries: StemmedWords, stemmer: Stemmer): AnalyzedReview = {

    val words = review.split(" ").map(_.toLowerCase(SupportedLanguages.getLocale(dictionaries.lang)))

    val stemmedReview = DictionaryStemmer.stemsForWords(words, stemmer)

    val total = words.size

    val positiveCount = stemmedReview.filter(stem => {
      dictionaries.positive.contains(stem)
    })

    val negativeCount = stemmedReview.filter(stem => {
      dictionaries.negative.contains(stem)
    })


    AnalyzedReview(review, positiveCount, negativeCount, total, stemmedReview)
  }

}

case class AnalyzedReview(review: String, positiveWords: Seq[String], negativeWords: Seq[String], totalWords: Int, stems: Seq[String]) {
  lazy val positiveWordsCount = positiveWords.size
  lazy val negativeWordsCount = negativeWords.size
}
