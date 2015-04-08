package org.allenai.scholar.metrics.metadata

import java.io.File

import org.allenai.scholar.metrics.{ ErrorAnalysis, PR }
import org.allenai.scholar.{ Author, MetadataAndBibliography, PaperMetadata }

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
    val goldMetadata = groundTruthMetadata.filterKeys(idFilter)
    val predictedMetadata = predictions.toMap.mapValues(_.metadata)
    val metadataMetrics = MetadataErrorAnalysis.computeMetrics(goldMetadata, predictedMetadata)
    val predictedBibs = predictions.toMap.mapValues(_.bibs.toSet)
    val goldBibs = groundTruthBibs.filterKeys(idFilter).mapValues(_.values.toSet)
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
    def computeF1(p: Double, r: Double): Double = if (r + p == 0.0) -1.0 else (2.0 * r * p)/(r + p)
    val analysis = computeEval(groundTruthMetadata, groundTruthBibs, idFilter)
    writeToFile(s"${algoName}-summary.txt") { w =>
      w.println("Metric\tPrecision\tRecall\tF1")
      for (ErrorAnalysis(metric, PR(p, r), _) <- analysis) {
        w.println(s"$metric\t${p.getOrElse("")}\t${r.getOrElse("")}\t${computeF1(p.getOrElse(0.0), r.getOrElse(0.0))}")
      }
    }
    val detailsDir = new File(s"${algoName}-details")
    detailsDir.mkdirs()
    def format(a: Any): String = a match {
      case a: Author => a.productIterator.map(format).filter(_.size > 0).mkString(" ")
      case m: PaperMetadata => s"${m.authors.map(_.lastName).mkString(" & ")} ${m.year}"
      case p: Product =>
        p.productIterator.map(format).mkString(",")
      case i: Iterable[_] => i.map(format).mkString(" ")
      case _ => a.toString
    }
    for (ErrorAnalysis(metric, _, examples) <- analysis) {
      writeToFile(new File(detailsDir, s"$metric.txt").getCanonicalPath) { w =>
        w.println("id\tPrecision\tRecall\tF1\tTruth\tPredicted")
        for ((id, ex) <- examples) {
          val truth = ex.trueLabels.map(format).mkString("|")
          val predictions = ex.predictedLabels.map(format).mkString("|")
          val PR(p, r) = ex.precisionRecall
          val f1 = computeF1(p.getOrElse(0.0), r.getOrElse(0.0))
          w.println(s"$id\t${p.getOrElse("")}\t${r.getOrElse("")}\t$f1\t$truth\t$predictions")
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
    val bibs = MetadataAndBibliography.edgesToBibKeyMap(citationEdges, groundTruthMetadata)
    idWhiteListFile match {
      case Some(fn) if new File(fn).exists =>
        val whiteList = Source.fromFile(fn).getLines.toSet
        run(groundTruthMetadata, bibs, whiteList.contains(_))
      case _ => run(groundTruthMetadata, bibs, id => true)
    }
  }
}
