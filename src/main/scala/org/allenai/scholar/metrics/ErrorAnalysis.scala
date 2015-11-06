package org.allenai.scholar.metrics

import java.io.{ File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.scholar.metrics.PrecisionRecall._

case class ErrorAnalysis(
  metricName: String,
  pr: PR,
  details: Iterable[(String, Example[_])]
)

object ErrorAnalysis {
  def computeMetrics[T](
    truth: Seq[(String, T)],
    predicted: Map[String, T],
    metrics: (String, T => Iterable[_])*
  ) = {

    val examples: Iterable[(String, (String, Example[_]))] =
      for {
        (id, trueData) <- truth
        predictedData = predicted.get(id)
        (metric, extract) <- metrics
      } yield {
        val trueLabels = extract(trueData)
        val predictedLabels: Iterable[_] = predictedData match {
          case Some(p) => extract(p)
          case _ => None
        }
        (metric, (id, Example(trueLabels, predictedLabels)))
      }
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2))
      ErrorAnalysis(metric, pr, examples)
    }
  }
}

object ErrorAnalysisIo {

  def write(data: Iterable[ErrorAnalysis], dir: File): Unit = {

    def format(d: Option[Double]) = d.map(_.toString).getOrElse("")
    def writeFile(name: String)(f: PrintWriter => Unit) = {
      val file = new File(dir, name)
      require((file.getParentFile.exists && file.getParentFile.isDirectory)
        || file.getParentFile.mkdirs(), s"Unable to find or create directory $dir")
      Resource.using(new PrintWriter(file)) {
        w => f(w)
      }
    }
    writeFile("PR.txt") { w =>
      w.println(s"Metric\tPrecision\tRecall")
      for (a <- data) {
        val p = format(a.pr.precision)
        val r = format(a.pr.recall)
        w.println(s"${a.metricName}\t$p\t$r")
      }
    }
    for (a <- data) {
      writeFile(s"details/${a.metricName}-labels.txt") { w =>
        w.println("ID\tAccuracy\tLabelType\tLabel")
        for ((id, ex) <- a.details) {
          val t = ex.trueLabels.map(_.toString).toSet
          val p = ex.predictedLabels.map(_.toString).toSet
          for (l <- t.toArray.sorted) {
            w.println(s"$id\t${if (p.contains(l)) 1 else 0}\ttrue\t$l")
          }
          for (l <- p.toArray.sorted) {
            w.println(s"$id\t${if (t.contains(l)) 1 else 0}\tpred\t$l")
          }
        }
      }
    }
  }
}

