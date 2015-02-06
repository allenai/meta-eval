package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.metrics.metadata.CoreMetadata._
import org.allenai.scholar.metrics.metadata.CoreMetadataMatch._

import java.io.File

case class Eval(
  algoName: String,
  taggedFiles: Array[File],
  taggedFileParser: File => CoreMetadata) {
  def computeEval(groundTruthMetadata: Map[String, CoreMetadata]): Array[CoreMetadataMatch] =
    taggedFiles.flatMap {
      f =>
        val id = f.getName.split('.')(0)
        groundTruthMetadata get id match {
          case Some(gt) => Some(matchCoreMetadata(id, taggedFileParser(f), gt))
          case _ => None
        }
    }

  def run(groundTruthMetadata: Map[String, CoreMetadata]) = {
    val matches = computeEval(groundTruthMetadata)
    println(s"Summary of match results for $algoName: ")
    CoreMetadataMatchStats.printSummary(CoreMetadataMatch.stats(matches))

    val evalTsvFile = algoName + ".tab"
    println(s"Saving the match results to $evalTsvFile in tab separated format ...")
    val entries = Seq(CoreMetadataMatch.getHeaderRow) ++ matches.map(_.getValueRow(algoName))
    writeToFile(entries, evalTsvFile)
  }
}

object Eval {
  def evalGrobid(files: Array[File], groundTruthMetadata: Map[String, CoreMetadata]) = {
    val grobidEval = Eval(
      "Grobid",
      files,
      parseGrobidCoreMetadata _)

    println(s"Processing ${files.size} grobid files")
    grobidEval.run(groundTruthMetadata)
  }

  def evalMetatagger(files: Array[File], groundTruthMetadata: Map[String, CoreMetadata]) = {
    val metataggerEval = Eval(
      "Metatagger",
      files,
      parseMetataggerCoreMetadata _)

    println(s"Processing ${files.size} metatagger files")
    metataggerEval.run(groundTruthMetadata)
  }
}
