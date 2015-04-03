package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar._
import org.allenai.scholar.metrics.PrecisionRecall._

case class ErrorAnalysis(
  metricName: String,
  precision: Option[Double],
  recall: Option[Double],
  details: Iterable[(String, PredictionAndTruth[_])]
)

object ErrorAnalysis {
  def computeMetrics[T](
    truth: Map[String, T],
    predicted: Map[String, T],
    metrics: (String, T => Iterable[_])*
  ) = {

    val examples: Iterable[(String, (String, PredictionAndTruth[_]))] =
      for {
        (id, trueData) <- truth.toList
        predictedData = predicted.get(id)
        (metric, extract) <- metrics
      } yield {
        val trueLabels = extract(trueData)
        val predictedLabels: Iterable[_] = predictedData match {
          case Some(p) => extract(p)
          case _ => None
        }
        (metric, (id, PredictionAndTruth.noConfidence(trueLabels, predictedLabels)))
      }
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2)).head
      ErrorAnalysis(metric, pr.precision, pr.recall, examples)
    }
  }
}

object PaperMetadataErrorAnalysis {
  def computeMetrics(truth: Map[String, PaperMetadata], predictions: Map[String, PaperMetadata]) =
    ErrorAnalysis.computeMetrics(truth, predictions,
      YEAR -> extractYear _,
      VENUE_NONEMPTY -> extractVenueNonEmpty _,
      AUTHOR_FULL_NAME -> extractAuthorExact _,
      AUTHOR_NORMALIZED_LAST_NAME -> extractAuthorLastName _,
      TITLE_EXACT -> extractTitleExact _,
      TITLE_NORMALIZED -> extractTitleNormalized _,
      TITLE_NONEMPTY -> extractTitleNonempty _)

  def extractYear(data: PaperMetadata): Iterable[Year] = PublicationYear.ifDefined(data.year)

  def extractVenueNonEmpty(data: PaperMetadata): Iterable[Venue] = data.venue.nonEmpty.ifDefined

  def extractAuthorExact(data: PaperMetadata): Iterable[Author] = data.authors

  def extractAuthorLastName(data: PaperMetadata): Iterable[Author] = data.authors.map(_.lastNameOnly.normalized)

  def extractTitleExact(data: PaperMetadata): Iterable[Title] = data.title.ifDefined

  def extractTitleNormalized(data: PaperMetadata): Iterable[Title] = data.title.normalized.ifDefined

  def extractTitleNonempty(data: PaperMetadata): Iterable[Title] = data.title.nonEmpty.ifDefined

  private val YEAR = "year"
  private val AUTHOR_FULL_NAME = "authorFullName"
  private val AUTHOR_NORMALIZED_LAST_NAME = "authorLastNameNormalized"
  private val TITLE_EXACT = "titleExact"
  private val TITLE_NORMALIZED = "titleNormalized"
  private val TITLE_NONEMPTY = "titleNonEmpty"
  private val VENUE_NONEMPTY = "venueNonEmpty"

}

