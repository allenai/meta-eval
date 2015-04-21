package org.allenai.scholar.metrics.metadata

import java.io.File

import org.allenai.scholar.metrics.{ ErrorAnalysis, PR }
import org.allenai.scholar.{ Author, MetadataAndBibliography }

import scala.io.Source

/** Eval objects define evaluations of different metadata extraction algorithms.
  * @param algoName The extraction algorithm's name.
  * @param taggedFiles Raw extracted files output by the algorithm.
  * @param taggedFileParser Function that parses an extracted file to produce core metadata.
  */
case class Eval(
    algoName: String,
    taggedFiles: Array[File],
    taggedFileParser: File => Option[MetadataAndBibliography]
) {
  def computeEval(
    groundTruthMetadata: Map[String, PaperMetadata],
    groundTruthBibs: Map[String, Map[String, PaperMetadata]],
    idFilter: String => Boolean
  ): Iterable[ErrorAnalysis] = {
    val predictions = for {
      f <- taggedFiles
      id = f.getName.split('.')(0)
      if idFilter(id)
      predicted <- taggedFileParser(f)
    } yield (id, predicted)
    val goldMetadata = groundTruthMetadata.filterKeys(idFilter).toList
    val predictedMetadata = predictions.toMap.mapValues(_.metadata)
    val metadataMetrics = MetadataErrorAnalysis.computeMetrics(goldMetadata, predictedMetadata)
    val predictedBibs = predictions.toMap.mapValues(_.bibs.toSet)
    val goldBibs = groundTruthBibs.filterKeys(idFilter).mapValues(_.values.toSet).toList
    val bibliographyMetrics = BibliographyErrorAnalysis.computeMetrics(goldBibs, predictedBibs)
    metadataMetrics ++ bibliographyMetrics
  }

  /** Run evaluation, print out summary, and save match data to Tableau format.
    * @param groundTruthMetadata map paper ids to ground truth core metadata.
    * @param groundTruthBibs map of paper ids to a map of "bibKey" to cited core metadata
    * @param idFilter only keep paper ids matching this filter
    */
  def run(
    groundTruthMetadata: Map[String, PaperMetadata],
    groundTruthBibs: Map[String, Map[String, PaperMetadata]],
    idFilter: String => Boolean
  ): Unit = {
    def computeF1(precision: Option[Double], recall: Option[Double]) = (precision, recall) match {
      case (Some(_), Some(0)) | (Some(0), Some(_)) => Some(0.0)
      case (Some(p), Some(r)) => Some((2.0 * r * p) / (r + p))
      case _ => None
    }
    val analysis = computeEval(groundTruthMetadata, groundTruthBibs, idFilter)
    writeToFile(s"${algoName}-summary.txt") { w =>
      w.println("Metric\tPrecision\tRecall\tF1")
      for (ErrorAnalysis(metric, PR(p, r), _) <- analysis) {
        val f1 = computeF1(p, r)
        w.println(s"$metric\t${p.getOrElse("")}\t${r.getOrElse("")}\t${f1.getOrElse("")}")
      }
    }
    val detailsDir = new File(s"${algoName}-details")
    detailsDir.mkdirs()
    def format(a: Any): String =
      if (Config.verboseLabelFormat) {
        a.toString
      } else {
        a match {
          case a: Author => a.productIterator.map(format).filter(_.size > 0).mkString(" ")
          case m: PaperMetadata => s"${m.authors.map(_.lastName).mkString(" & ")} ${m.year}"
          case p: Product =>
            p.productIterator.map(format).mkString(",")
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
          val report = Seq(id, p.getOrElse(""), r.getOrElse(""), f1.getOrElse(""), falsePositives,
            falseNegatives, truth, predictions).mkString("\t")
          w.println(report)
        }
      }
    }
  }

  def run(
    groundTruthMetadataFile: String,
    groundTruthCitationEdgesFile: String,
    idWhiteListFile: Option[String] = None
  ): Unit = {
    import PaperMetadata._
    val groundTruthMetadata = fromJsonLinesFile(groundTruthMetadataFile)
    val citationEdges = for {
      line <- Source.fromFile(groundTruthCitationEdgesFile).getLines.toIterable
      s = line.split('\t')
      if s.length > 1
    } yield {
      (s(0), s(1))
    }
    var bibs = MetadataAndBibliography.edgesToBibKeyMap(citationEdges, groundTruthMetadata)
    idWhiteListFile match {
      case Some(fn) if new File(fn).exists =>
        val whiteList = Source.fromFile(fn).getLines.toSet
        for (id <- whiteList -- bibs.keySet) {
          bibs += (id -> Map())
        }
        run(groundTruthMetadata, bibs, whiteList.contains(_))
      case _ => run(groundTruthMetadata, bibs, id => true)
    }
  }
}
