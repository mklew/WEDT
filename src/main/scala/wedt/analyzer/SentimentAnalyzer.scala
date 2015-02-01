package wedt.analyzer

import wedt.crawler.SupportedLanguages

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object SentimentAnalyzer {

  private def doAnalyze(review: String, total: Int, dictionaries: StemmedWords, stemmedReview: scala.Seq[String]) = {
    def isNegation(w: String) = w == "nie" || w == "not"

    val checked = stemmedReview.map(stem => (stem, dictionaries.positive.contains(stem), dictionaries.negative.contains(stem)))
    val sentimented = checked.zipWithIndex.map {case (w, i) => if((i > 1 && isNegation(checked(i-2)._1) ) ||
      (i > 0 && isNegation(checked(i-1)._1))) (w._1, false, false) else w} // negation

    val positiveUnique = sentimented.filter(x => x._2 && !x._3).map(_._1)
    val negativeUnique = sentimented.filter(x => !x._2 && x._3).map(_._1)
    val positiveAndNegative = sentimented.filter(x => x._2 && x._3).map(_._1)
    val neutral = sentimented.filter(x => !x._2 && !x._3).map(_._1)

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

