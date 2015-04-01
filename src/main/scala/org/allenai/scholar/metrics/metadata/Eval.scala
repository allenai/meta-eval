package org.allenai.scholar.metrics.metadata

import java.io.File

import org.allenai.scholar.MetadataAndBibliography

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
    MetadataErrorAnalysis(goldMetadata, predictedMetadata)
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
    val analysis = computeEval(groundTruthMetadata, groundTruthBibs, idFilter)
  }

  def run(
    groundTruthMetadataFile: String,
    groundTruthCitationEdgesFile: String,
    idWhiteListFile: Option[String] = None
  ): Unit = {
    import org.allenai.scholar.metrics.metadata.PaperMetadata._
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
      case Some(fn) =>
        val whiteList = Source.fromFile(fn).getLines.toSet
        run(groundTruthMetadata, bibs, whiteList.contains(_))
      case None => run(groundTruthMetadata, bibs, id => true)
    }
  }
}
