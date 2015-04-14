package org.allenai.scholar.metrics

import scala.collection.mutable.ArrayBuffer

/** Generalization of Precision/Recall measurement to multi-label problems.
  * For each example, there is a set of predictions P and a set of true labels T
  * Each example is assigned a precision/recall pair
  * precision = | P intersect T | / |P|
  * recall = | P intersect T | / |T|
  *
  * Either precision or recall may be undefined if P or T is empty
  *
  * The overall precision/recall is the average over examples for which precision/recall is defined
  */
object PrecisionRecall {

  // A threshold and the precision/recall for that threshold
  case class PRAtThreshold(threshold: Double, precision: Option[Double], recall: Option[Double])

  def measurePR[T](data: Iterable[Example[T]]) = {
    val scoredExamples = data.map(ex => ScoredExample(
      ex.trueLabels,
      ex.predictedLabels.map(l => (l, 1.0))
    ))
    val pr = scanConfidenceMeasurePR(scoredExamples).head
    PR(pr.precision, pr.recall)
  }

  // Given a set of examples, with predictions and truth for each example, compute the global P/R
  // at a set of confidence thresholds
  def scanConfidenceMeasurePR[T](
    data: Iterable[ScoredExample[T]],
    nBins: Int = 10000
  ): Seq[PRAtThreshold] = {
    // Efficient algorithm as follows:
    //   For each example, initialize an empty confusion matrix
    //   Scan through all the predicted labels once, ordered by confidence descending
    //   Update each example's confusion matrix, and the global P/R number
    //   Output the global P/R at each threshold checkpoint
    val confidences = (for {
      ScoredExample(truth, predictedLabels) <- data
      matrix = new ConfusionMatrix(0, 0, truth.size)
      (predictedLabel, confidence) <- predictedLabels
    } yield {
      (confidence, predictedLabel, truth.toSet, matrix)
    }).toVector.sortBy(-_._1)

    // Normal case (ground truth exists)
    if (data.exists(_.trueLabels.size > 0)) {
      val thresholds = if (confidences.size <= nBins) {
        confidences.map(_._1)
      } else {
        val (minConfidence, maxConfidence) = confidences.map(_._1).foldLeft((
          Double.PositiveInfinity,
          Double.NegativeInfinity
        )) {
          case ((min, max), conf) => (math.min(min, conf), math.max(max, conf))
        }
        (nBins to 1 by -1).map(bin => minConfidence + bin * (maxConfidence - minConfidence) / nBins)
      }

      val prAtThreshold = new ArrayBuffer[PRAtThreshold]

      var i = 0
      val accum = new PRAccumulator(data.count(_.trueLabels.size > 0))
      for (t <- thresholds) {
        for (
          (confidence, predictedLabel, truth, matrix) <- confidences.drop(i).takeWhile(_._1 >= t)
        ) {
          i += 1
          val oldPr = matrix.pr
          if (truth.contains(predictedLabel)) {
            matrix.addTruePositive()
          } else {
            matrix.addFalsePositive()
          }
          val newPr = matrix.pr
          accum.updateExample(oldPr, newPr)
        }
        val PR(p, r) = accum.globalPR
        prAtThreshold.append(PRAtThreshold(t, p, r))
      }

      prAtThreshold.size match {
        // No examples have predicted labels
        case 0 => List(PRAtThreshold(0.0, None, Some(0.0)))
        // Normal case
        case _ => prAtThreshold.toVector
      }
    } else {
      // Special case for when no examples have any positive labels.
      // In this case, precision is always zero and recall is undefined
      confidences.headOption.map(_._1) match {
        case Some(maxConfidence) => List(PRAtThreshold(maxConfidence, Some(0.0), None))
        // Completely pathological case in which there is no truth and no predictions
        case None => List(PRAtThreshold(0.0, None, None))
      }
    }
  }

  // Mutable-state confusion matrix class for accumulating precision/recall values
  private class ConfusionMatrix(var truePositives: Int, var positivePredictionCount: Int,
      trueLabelCount: Int) {
    def addTruePositive() = {
      truePositives += 1
      positivePredictionCount += 1
    }

    def addFalsePositive() = positivePredictionCount += 1

    def pr = {
      val precision = if (positivePredictionCount > 0) {
        Some(truePositives.toDouble / positivePredictionCount)
      } else {
        None
      }

      val recall = if (trueLabelCount > 0) {
        Some(truePositives.toDouble / trueLabelCount)
      } else {
        None
      }
      PR(precision, recall)
    }
  }

  // Mutable-state class for accumulating global precision/recall measurements
  // from a collection of examples
  private class PRAccumulator(_recallTotal: Int) {
    // Total number of examples with a defined precision
    // (This will change if the predicted labels change for that example)
    private var precisionTotal: Int = 0
    // Sum of precision numbers for all examples for which precision is defined
    private var precisionSum: Double = 0.0
    // Total number of examples with a defined recall
    // (This should never change)
    private val recallTotal: Int = _recallTotal
    // Sum of recall numbers for all examples for which recall is defined
    private var recallSum: Double = 0.0

    // Update the global P/R numbers based on a change in predicted labels
    // for a single example
    def updateExample(oldPR: PR, newPR: PR) = {
      updateExamplePrecision(oldPR.precision, newPR.precision)
      updateExampleRecall(oldPR.recall, newPR.recall)
    }

    def updateExamplePrecision(oldPrecision: Option[Double], newPrecision: Option[Double]) = {
      (oldPrecision, newPrecision) match {
        case (None, Some(p)) =>
          precisionTotal += 1
          precisionSum += p
        case (Some(pOld), Some(pNew)) =>
          precisionSum += pNew - pOld
        case (None, None) => ()
        case (Some(_), None) => sys.error("P/R error: transition from defined to undefined")
      }
    }

    def updateExampleRecall(oldRecall: Option[Double], newRecall: Option[Double]) = {
      (oldRecall, newRecall) match {
        case (Some(rOld), Some(rNew)) =>
          recallSum += rNew - rOld
        case (None, None) => ()
        case _ => sys.error("P/R error: recall should always be either defined or undefined")
      }
    }

    def globalPR: PR = {
      val p = if (precisionTotal > 0) Some(precisionSum / precisionTotal) else None
      val r = if (recallTotal > 0) Some(recallSum / recallTotal) else None
      PR(p, r)
    }
  }

}
