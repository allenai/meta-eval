package org.allenai.scholar.metrics.metadata

import com.sun.xml.internal.bind.v2.util.EditDistance
import spray.json.DefaultJsonProtocol._

import java.text.DecimalFormat

/** Object containing key stats of comparing core metadata.
  * @param nEntries Number of papers in the eval.
  * @param titleExact Precision of title using exact string matching.
  * @param titleEdit Average title edit distance (normalized by the longer title's length).
  * @param titleEditPrecision Normalized edit distance precision of title.
  * @param fullNameCountDiff Average difference between the number of authors.
  * @param fullNameExact Precision of full name extraction using exact string matching.
  * @param fullNameEdit Average full name edit distance (normalized by the longer name's length).
  * @param fullNameEditPrecision Normalized edit distance precision of full name.
  * @param lastNameExact Precision of last name extraction using exact string matching.
  * @param lastNameEdit Average last name edit distance (normalized by the longer name's length).
  * @param lastNameEditPrecision Normalized edit distance precision of last name.
  */
case class CoreMetadataMatchStats(
  nEntries: Int,
  titleExact: Double, titleEdit: Double, titleEditPrecision: Double,
  fullNameCountDiff: Double, fullNameExact: Double, fullNameEdit: Double, fullNameEditPrecision: Double,
  lastNameExact: Double, lastNameEdit: Double, lastNameEditPrecision: Double
)

object CoreMetadataMatchStats {
  val formatter = new DecimalFormat("#.##")

  def printSummary(matchSummary: CoreMetadataMatchStats) {
    println(s"Scoring performance on ${matchSummary.nEntries} papers: ")

    println(s"Precision on titles exact/edit: ${
      formatter.format(matchSummary.titleExact)
    }%/${formatter.format(matchSummary.titleEditPrecision)}%")

    println(s"Average difference in number of author full names: ${formatter.format(matchSummary.fullNameCountDiff)}")
    println(s"Precision on author full names exact/edit: ${
      formatter.format(matchSummary.fullNameExact)
    }%/ ${formatter.format(matchSummary.fullNameEditPrecision)}%")

    println(s"Precision on author last names exact/edit: ${
      formatter.format(matchSummary.lastNameExact)
    }%/${formatter.format(matchSummary.lastNameEditPrecision)}")
  }
}

case class CoreMetadataMatch(
    id: String,
    titleExact: Boolean,
    titleEdit: Double,
    authorFullNameExact: Boolean,
    authorFullNameCountDiff: Int,
    authorFullNameEdit: Double,
    authorLastNameExact: Boolean,
    authorLastNameCountDiff: Int,
    authorLastNameEdit: Double
) {

  /** Prepare a row in Tableau format for this match.
    * @param algoName
    * @return
    */
  def getValueRow(algoName: String) = {
    import CoreMetadataMatch._

    def boolToInt(b: Boolean) = if (b) 1 else 0

    algoName + sep + fields.map { f =>
      f.setAccessible(true)
      if (f.getType.getName == "boolean") boolToInt(f.getBoolean(this)) else f.get(this)
    }.mkString(sep)
  }
}

object CoreMetadataMatch {
  val sep = "\t"

  lazy val fields = classOf[CoreMetadataMatch].getDeclaredFields

  /** Prepare the header in Tableau format.
    * @return
    */
  def getHeaderRow() = "algorithm" + sep + fields.map(_.getName).mkString(sep)

  implicit val jsFormat = jsonFormat9(CoreMetadataMatch.apply)

  def matchCoreMetadata(id: String, m1: CoreMetadata, m2: CoreMetadata): CoreMetadataMatch = {
    def editDistance(s1: String, s2: String): Double = {
      val d = EditDistance.editDistance(s1, s2)
      if (d == 0) 0.0 else d * 1.0 / Seq(s1.length, s2.length).max
    }

    def matchLists(l1: Seq[String], l2: Seq[String]) = {
      val countDiff = l1.size - l2.size
      val exact = countDiff == 0 && l1.zip(l2).forall(pair => pair._1 == pair._2)
      val edit = editDistance(l1.mkString(";"), l2.mkString(";"))
      (countDiff, exact, edit)
    }

    val (fullNames1, fullNames2) = (m1.authorNames, m2.authorNames)
    val lastNames1 = fullNames1.map(_.lastNameFromFull)
    val lastNames2 = fullNames2.map(_.lastNameFromFull)

    val (fullNameCountDiff, fullNameExact, fullNameEdit) = matchLists(fullNames1, fullNames2)
    val (lastNameCountDiff, lastNameExact, lastNameEdit) = matchLists(lastNames1, lastNames2)

    CoreMetadataMatch(
      id = id,
      titleExact = m1.title == m2.title,
      titleEdit = editDistance(m1.title, m2.title),
      authorFullNameExact = fullNameExact,
      authorFullNameCountDiff = fullNameCountDiff,
      authorFullNameEdit = fullNameEdit,
      authorLastNameExact = lastNameExact,
      authorLastNameCountDiff = lastNameCountDiff,
      authorLastNameEdit = lastNameEdit
    )
  }

  def stats(matches: Seq[CoreMetadataMatch]) = {
    def editPrecision(edit: Double) = 100 - 100 * edit

    val nEntries = matches.size
    val titleExact = matches.count(m => m.titleExact) * 100.0 / nEntries
    val titleEdit = matches.map(_.titleEdit).sum / nEntries
    val titleEditPrecision = editPrecision(titleEdit)

    val fullNameCountDiff = matches.map(_.authorFullNameCountDiff).sum * 1.0 / nEntries
    val fullNameExact = matches.count(m => m.authorFullNameExact) * 100.0 / nEntries
    val fullNameEdit = matches.map(_.authorFullNameEdit).sum / nEntries
    val fullNameEditPrecision = editPrecision(fullNameEdit)

    val lastNameExact = matches.count(m => m.authorLastNameExact) * 100.0 / nEntries
    val lastNameEdit = matches.map(_.authorLastNameEdit).sum / nEntries
    val lastNameEditPrecision = editPrecision(lastNameEdit)

    CoreMetadataMatchStats(
      nEntries = nEntries,
      titleExact = titleExact, titleEdit = titleEdit, titleEditPrecision = titleEditPrecision,
      fullNameCountDiff = fullNameCountDiff, fullNameExact = fullNameExact, fullNameEdit = fullNameEdit, fullNameEditPrecision = fullNameEditPrecision,
      lastNameExact = lastNameExact, lastNameEdit = lastNameEdit, lastNameEditPrecision = lastNameEditPrecision
    )
  }
}

