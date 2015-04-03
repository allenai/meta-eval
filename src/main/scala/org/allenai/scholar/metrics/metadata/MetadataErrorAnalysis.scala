package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.PublicationYear
import org.allenai.scholar.metrics.PrecisionRecall._

object MetadataErrorAnalysis {
  def apply(
    truth: Map[String, PaperMetadata],
    predicted: Map[String, PaperMetadata]
  ) = {

    def extractPredicted[V](
      data: Option[PaperMetadata]
    )(
      extract: PaperMetadata => Iterable[V]
    ): Iterable[V] =
      data match {
        case Some(p) => extract(p)
        case None => List()
      }
    val examples: Iterable[(String, (String, PredictionAndTruth[_]))] =
      (for {
        (id, PaperMetadata(t, v, y, a)) <- truth
        predictedMeta = predicted.get(id)
      } yield {
        def predicted[T] =
          extractPredicted[T](predictedMeta) _
        List(
          YEAR ->
            PredictionAndTruth.noConfidence(PublicationYear.ifDefined(y), predicted(x => PublicationYear.ifDefined(x.year))),
          VENUE_NONEMPTY ->
            PredictionAndTruth.noConfidence(v.nonEmpty.ifDefined, predicted(_.venue.nonEmpty.ifDefined)),
          AUTHOR_FULL_NAME ->
            PredictionAndTruth.noConfidence(a, predicted(_.authors)),
          AUTHOR_NORMALIZED_LAST_NAME ->
            PredictionAndTruth.noConfidence(a.map(_.lastNameOnly.normalized), predicted(_.authors.map(_.lastNameOnly.normalized))),
          TITLE_EXACT ->
            PredictionAndTruth.noConfidence(t.ifDefined, predicted(x => x.title.ifDefined)),
          TITLE_NORMALIZED ->
            PredictionAndTruth.noConfidence(t.normalized.ifDefined, predicted(x => x.title.normalized.ifDefined)),
          TITLE_NONEMPTY ->
            PredictionAndTruth.noConfidence(t.nonEmpty.ifDefined, predicted(x => x.title.nonEmpty.ifDefined))
        ).map { case (metric, ex) => (metric, (id, ex)) }
      }).flatten
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2)).head
      ErrorAnalysis(metric, pr.precision, pr.recall, examples)
    }
  }

  private val YEAR = "year"
  private val AUTHOR_FULL_NAME = "authorFullName"
  private val AUTHOR_NORMALIZED_LAST_NAME = "authorLastNameNormalized"
  private val TITLE_EXACT = "titleExact"
  private val TITLE_NORMALIZED = "titleNormalized"
  private val TITLE_NONEMPTY = "titleNonEmpty"
  private val VENUE_NONEMPTY = "venueNonEmpty"

}

case class ErrorAnalysis(
  metricName: String,
  precision: Option[Double],
  recall: Option[Double],
  details: Iterable[(String, PredictionAndTruth[_])]
)
