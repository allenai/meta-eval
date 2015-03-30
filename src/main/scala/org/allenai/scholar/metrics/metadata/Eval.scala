package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.metrics.metadata.CoreMetadata._
import org.allenai.scholar.metrics.metadata.CoreMetadataMatch._

import scala.io.Source

import java.io.File

/** Eval objects define evaluations of different metadata extraction algorithms.
  * @param algoName The extraction algorithm's name.
  * @param taggedFiles Raw extracted files output by the algorithm.
  * @param taggedFileParser Function that parses an extracted file to produce core metadata.
  */
case class Eval(
    algoName: String,
    taggedFiles: Array[File],
    taggedFileParser: File => Option[CoreMetadata]
) {
  def computeEval(
    groundTruthMetadata: Map[String, CoreMetadata],
    groundTruthBibs: Map[String, Map[String, CoreMetadata]],
    idFilter: String => Boolean
  ): Array[CoreMetadataMatch] =
    for {
      f <- taggedFiles
      id = f.getName.split('.')(0)
      if idFilter(id)
      m <- taggedFileParser(f) match {
        case Some(parsed) =>
          groundTruthMetadata get id match {
            case Some(gt) => Some(matchCoreMetadata(id, parsed, gt, groundTruthBibs.get(id)))
            case _ => None
          }
        case None => None
      }
    } yield m

  /** Run evaluation, print out summary, and save match data to Tableau format.
    * @param groundTruthMetadata map paper ids to ground truth core metadata.
    * @param groundTruthBibs map of paper ids to a map of "bibKey" to cited core metadata
    * @param idFilter only keep paper ids matching this filter
    */
  def run(
    groundTruthMetadata: Map[String, CoreMetadata],
    groundTruthBibs: Map[String, Map[String, CoreMetadata]],
    idFilter: String => Boolean
  ): Unit = {
    val matches = computeEval(groundTruthMetadata, groundTruthBibs, idFilter)
    CoreMetadataMatchStats.printSummary(CoreMetadataMatch.stats(matches))

    val evalTsvFile = algoName + ".tab"
    println(s"Saving the match results to $evalTsvFile in tab separated format ...")
    val entries = Seq(CoreMetadataMatch.getHeaderRow) ++ matches.map(_.getValueRow(algoName))
    writeToFile(entries, evalTsvFile)
  }

  def run(
    groundTruthMetadataFile: String,
    groundTruthCitationEdgesFile: String,
    idWhiteListFile: Option[String] = None
  ): Unit = {
    import org.allenai.scholar.metrics.metadata.PaperMetadata._
    val groundTruthMetadata = convertToCore(fromJsonLinesFile(groundTruthMetadataFile))
    val citationEdges = for {
      line <- Source.fromFile(groundTruthCitationEdgesFile).getLines.toIterable
      s = line.split('\t')
      if s.length > 1
    } yield {
      (s(0), s(1))
    }
    val bibs = CoreMetadata.edgesToBibKeyMap(citationEdges, groundTruthMetadata)
    idWhiteListFile match {
      case Some(fn) =>
        val whiteList = Source.fromFile(fn).getLines.toSet
        run(groundTruthMetadata, bibs, whiteList.contains(_))
      case None => run(groundTruthMetadata, bibs, id => true)
    }
  }
}
