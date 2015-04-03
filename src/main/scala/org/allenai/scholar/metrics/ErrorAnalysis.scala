package org.allenai.scholar.metrics

import java.time.Year

import org.allenai.scholar._
import org.allenai.scholar.metrics.PrecisionRecall._

case class ErrorAnalysis(
  metricName: String,
  pr: PR,
  details: Iterable[(String, Example[_])]
)

object ErrorAnalysis {
  def computeMetrics[T](
    truth: Map[String, T],
    predicted: Map[String, T],
    metrics: (String, T => Iterable[_])*
  ) = {

    val examples: Iterable[(String, (String, Example[_]))] =
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
        (metric, (id, ScoredExample.noConfidence(trueLabels, predictedLabels)))
      }
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2))
      ErrorAnalysis(metric, pr, examples)
    }
  }
}

