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
    bibs: Map[String, Map[String, CoreMetadata]]
  ): Array[CoreMetadataMatch] =
    taggedFiles.flatMap {
      f =>
        taggedFileParser(f) match {
          case Some(parsed) =>
            val id = f.getName.split('.')(0)
            groundTruthMetadata get id match {
              case Some(gt) => Some(matchCoreMetadata(id, parsed, gt, bibs.get(id)))
              case _ => None
            }
          case None => None
        }
    }

  /** Run evaluation, print out summary, and save match data to Tableau format.
    * @param groundTruthMetadata map paper ids to ground truth core metadata.
    */
  def run(
    groundTruthMetadata: Map[String, CoreMetadata],
    bibs: Map[String, Map[String, CoreMetadata]]
  ): Unit = {
    val matches = computeEval(groundTruthMetadata, bibs)
    println(s"Summary of match results for $algoName: ")
    CoreMetadataMatchStats.printSummary(CoreMetadataMatch.stats(matches))

    val evalTsvFile = algoName + ".tab"
    println(s"Saving the match results to $evalTsvFile in tab separated format ...")
    val entries = Seq(CoreMetadataMatch.getHeaderRow) ++ matches.map(_.getValueRow(algoName))
    writeToFile(entries, evalTsvFile)
  }

  def run(groundTruthMetadataFile: String, citationEdgesFile: String): Unit = {
    import org.allenai.scholar.metrics.metadata.PaperMetadata._
    val groundTruthMetadata = convertToCore(fromJsonLinesFile(groundTruthMetadataFile))

    val edges = for {
      line <- Source.fromFile(citationEdgesFile).getLines
      s = line.split('\t')
      if s.length > 1
      (citing, citee) = (s(0), s(1))
      citeeMeta <- groundTruthMetadata.get(citee) match {
        case Some(cm) => Some(bibKey(cm), cm)
        case None => None
      }
    } yield {
      citing -> citeeMeta
    }

    val bibs = edges.toList
      .groupBy(_._1) // group by citing paper id
      .mapValues(_.map(_._2).toMap) // each value is a map from citee's bibKey to its CoreMetadata
    run(groundTruthMetadata, bibs)
  }
}

/** Defining evaluations of different metadata extraction algorithms.
  */
object Eval {
  def evalGrobid(files: Array[File], groundTruthMetadataFile: String, citationEdgesFile: String) = {
    val grobidEval = Eval(
      "Grobid",
      files,
      grobidParser.parseCoreMetadata
    )

    println(s"Processing ${files.size} grobid files")
    grobidEval.run(groundTruthMetadataFile, citationEdgesFile)
  }

  def evalMetatagger(files: Array[File], groundTruthMetadataFile: String, citationEdgesFile: String) = {
    val metataggerEval = Eval(
      "Metatagger",
      files,
      metataggerParser.parseCoreMetadata
    )

    println(s"Processing ${files.size} metatagger files")
    metataggerEval.run(groundTruthMetadataFile, citationEdgesFile)
  }
}
