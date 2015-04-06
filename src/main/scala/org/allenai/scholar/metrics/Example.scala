package org.allenai.scholar.metrics

trait Example[+T] {
  val trueLabels: Iterable[T]
  val predictedLabels: Iterable[T]

  private lazy val overlap =
    predictedLabels.toSet.intersect(trueLabels.toSet).size.toDouble
  lazy val precisionRecall = {
    val p = if (predictedLabels.size > 0) {
      Some(overlap / predictedLabels.size)
    } else {
      None
    }

    val r =
      if (trueLabels.size > 0) {
        Some(overlap / trueLabels.size)
      } else {
        None
      }
    PR(p, r)
  }
}

object Example {
  def apply[T](truth: Iterable[T], predictions: Iterable[T]): Example[T] =
    new Example[T] {
      val trueLabels = truth
      val predictedLabels = predictions
    }
}

// True labels and predicted labels with confidence
case class ScoredExample[+T](
    trueLabels: Iterable[T],
    scoredPredictions: Iterable[(T, Double)]
) extends Example[T] {
  val predictedLabels = scoredPredictions.map(_._1)
}

object ScoredExample {
  def noConfidence[T](trueLabels: Iterable[T], predictedLabels: Iterable[T]) = Example(trueLabels, predictedLabels)
}

