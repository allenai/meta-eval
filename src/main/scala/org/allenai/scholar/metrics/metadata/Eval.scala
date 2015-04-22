package org.allenai.scholar.metrics.metadata

import java.io.File
import java.text.DecimalFormat

import org.allenai.scholar.metrics.{ ErrorAnalysis, PR }
import org.allenai.scholar.{ Author, MetadataAndBibliography }

import scala.collection.immutable
import scala.io.Source

object Eval {

  /** Eval objects define evaluations of different metadata extraction algorithms.
    * @param algoName The extraction algorithm's name.
    * @param predictedMetadata The predict metadata.
    * @param predictedBibs The predicted bibs.
    * @param goldMetadata map paper ids to ground truth core metadata.
    * @param goldBibs map of paper ids to a map of "bibKey" to cited core metadata
    */
  def run(
    algoName: String,
    predictedMetadata: Map[String, PaperMetadata],
    predictedBibs: Map[String, Set[PaperMetadata]],
    goldMetadata: Seq[(String, PaperMetadata)],
    goldBibs: Seq[(String, Set[PaperMetadata])]
  ): Unit = {
    val metadataMetrics = MetadataErrorAnalysis.computeMetrics(goldMetadata, predictedMetadata)
    val bibliographyMetrics = BibliographyErrorAnalysis.computeMetrics(goldBibs, predictedBibs)
    val analysis = metadataMetrics ++ bibliographyMetrics
    writeSummary(algoName, analysis)
    writeDetails(algoName, analysis)
  }

  def run(
    algoName: String,
    parser: Parser,
    extractedDir: String,
    groundTruthMetadataFile: String,
    groundTruthCitationEdgesFile: String,
    idWhiteListFile: Option[String] = None
  ): Unit = {
    import PaperMetadata._
    val whiteList: Set[String] = idWhiteListFile match {
      case Some(fn) if new File(fn).exists => Source.fromFile(fn).getLines.toSet
      case _ => Set.empty
    }

    def idFilter(id: String): Boolean = whiteList.isEmpty || whiteList.contains(id)

    val predictions = parser.parseFromExtractedDir(extractedDir, idFilter)
    val groundTruthMetadata = fromJsonLinesFile(groundTruthMetadataFile)
    val citationEdges = for {
      line <- Source.fromFile(groundTruthCitationEdgesFile).getLines.toIterable
      s = line.split('\t')
      if s.length > 1
    } yield (s(0), s(1))

    var bibs = MetadataAndBibliography.edgesToBibKeyMap(citationEdges, groundTruthMetadata)
    (whiteList -- bibs.keySet).foreach(id => bibs += (id -> Set()))
    run(
      algoName = algoName,
      predictedMetadata = predictions.mapValues(_.metadata),
      predictedBibs = predictions.mapValues(_.bibs.toSet),
      goldMetadata = groundTruthMetadata.filterKeys(idFilter).toList,
      goldBibs = bibs.filterKeys(idFilter).toList
    )
  }

  val formatter = new DecimalFormat("#.##")
  def format(n: Option[Double]) =
    if (n.isDefined) formatter.format(n.get) else ""

  private def writeDetails(algoName: String, analysis: immutable.Iterable[ErrorAnalysis]): Unit = {
    val detailsDir = new File(s"${algoName}-details")
    detailsDir.mkdirs()
    def format(a: Any): String =
      if (Config.verboseLabelFormat) {
        a.toString
      } else {
        a match {
          case a: Author => a.productIterator.map(format).filter(_.size > 0).mkString(" ")
          case m: PaperMetadata => s"${m.authors.map(_.lastName).mkString(" & ")} ${m.year}"
          case p: Product => p.productIterator.map(format).mkString(",")
          case i: Iterable[_] => i.map(format).mkString(" ")
          case _ => a.toString
        }
      }

    for (ErrorAnalysis(metric, _, examples) <- analysis) {
      writeToFile(new File(detailsDir, s"$metric.txt").getCanonicalPath) { w =>
        w.println("id\tPrecision\tRecall\tF1\tFalsePositives\tFalseNegatives\tTruth\tPredicted")
        for ((id, ex) <- examples) {
          val truth = ex.trueLabels.map(format).mkString("|")
          val predictions = ex.predictedLabels.map(format).mkString("|")
          val falsePositives = (ex.predictedLabels.toSet -- ex.trueLabels).map(format).mkString("|")
          val falseNegatives = (ex.trueLabels.toSet -- ex.predictedLabels).map(format).mkString("|")
          val PR(p, r) = ex.precisionRecall
          val f1 = computeF1(p, r)
          val report = Seq(id, format(p), format(r), format(f1), falsePositives,
            falseNegatives, truth, predictions).mkString("\t")
          w.println(report)
        }
      }
    }
  }

  private def writeSummary(algoName: String, analysis: immutable.Iterable[ErrorAnalysis]): Unit = {
    writeToFile(s"${algoName}-summary.txt") { w =>
      w.println("Metric\tPrecision\tRecall\tF1")
      for (ErrorAnalysis(metric, PR(p, r), _) <- analysis) {
        val values = Seq(p, r, computeF1(p, r)).map(format).mkString("\t")
        w.println(s"$metric\t$values")
      }
    }
  }

  private def computeF1(precision: Option[Double], recall: Option[Double]) =
    (precision, recall) match {
      case (Some(_), Some(0)) | (Some(0), Some(_)) => Some(0.0)
      case (Some(p), Some(r)) => Some((2.0 * r * p) / (r + p))
      case _ => None
    }

}
