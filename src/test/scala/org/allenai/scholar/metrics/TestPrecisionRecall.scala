package org.allenai.scholar.metrics

import org.allenai.common.testkit.UnitSpec

class TestPrecisionRecall extends UnitSpec {

  def compareDoubleOption(measured: Option[Double], expected: Option[Double]) = {
    (measured, expected) match {
      case (Some(m), Some(e)) => m should equal(e +- .001)
      case _ => measured should equal(expected)
    }
  }

  "ErrorAnalysis" should "compute simple P/R correctly" in {
    val truth = Seq("label1" -> "value1", "label2" -> "value2")
    val predictions = Map("label1" -> "value1", "label2" -> "value2Wrong")
    val errorAnalysis = ErrorAnalysis.computeMetrics(
      truth, predictions, ("string comparison", (s: String) => List(s))
    ).head
    val pr = errorAnalysis.pr
    compareDoubleOption(pr.precision, Some(0.5))
    compareDoubleOption(pr.recall, Some(0.5))
  }

  "ErrorAnalysis" should "handle multiple labels" in {
    val truth = Seq(
      "key1" -> Seq("value1.0", "value1.1"),
      "key2" -> Seq("value2.0", "value2.1")
    )
    val predictions = Map(
      "key1" -> Seq("value1.0", "value1.1Wrong", "value1.2Wrong"),
      "key2" -> Seq("value2.0")
    )
    val errorAnalysis = ErrorAnalysis.computeMetrics(
      truth, predictions, ("set compare", (seq: Seq[String]) => seq.toSet)
    ).head
    val pr = errorAnalysis.pr
    compareDoubleOption(pr.precision, Some(2.toDouble / 3))
    compareDoubleOption(pr.recall, Some(1.toDouble / 2))
  }

}
