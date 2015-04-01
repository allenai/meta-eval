package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.metrics.PrecisionRecall._


object MetadataErrorAnalysis {
  def apply(
      truth: Map[String, PaperMetadata],
      predicted: Map[String, PaperMetadata]
      ) = {

    def extractPredicted[V](
        data: Option[PaperMetadata])(
        extract: PaperMetadata => Iterable[V]): Iterable[V] =
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
                PredictionAndTruth.noConfidence(List(y), predicted(x => List(x.year))),
            AUTHOR_FULL_NAME ->
                PredictionAndTruth.noConfidence(a, predicted(_.authors)),
            AUTHOR_LAST_NAME ->
                PredictionAndTruth.noConfidence(a.map(_.lastNameOnly), predicted(_.authors.map(_.lastNameOnly))),
            TITLE_EXACT ->
                PredictionAndTruth.noConfidence(List(t), predicted(x => List(x.title))),
            TITLE_NORMALIZED ->
                PredictionAndTruth.noConfidence(List(t.normalized), predicted(x => List(x.title.normalized))),
            TITLE_NONEMPTY ->
                PredictionAndTruth.noConfidence(List(t.nonEmpty), predicted(x => List(x.title.nonEmpty)))
          ).map(x => (id, x))
        }).flatten
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2)).head
      ErrorAnalysis(metric, pr.precision, pr.recall, examples)
    }
  }

  private val YEAR = "year"
  private val AUTHOR_FULL_NAME = "authorFullName"
  private val AUTHOR_LAST_NAME = "authorLastName"
  private val TITLE_EXACT = "titleExact"
  private val TITLE_NORMALIZED = "titleNormalized"
  private val TITLE_NONEMPTY = "titleNonEmpty"

}

case class ErrorAnalysis(
    metricName: String,
    precision: Option[Double],
    recall: Option[Double],
    details: Iterable[(String, PredictionAndTruth[_])]
    )
