package org.allenai.scholar.metrics.metadata

import com.sun.xml.internal.bind.v2.util.EditDistance

import java.text.DecimalFormat

/** Object containing key stats of comparing core metadata.
  * @param nEntries Number of papers in the eval.
  * @param titleExact Precision of title using exact string matching.
  * @param titleEdit Normalized edit distance precision of title.
  * @param fullNameCountDiff Average difference between the number of authors.
  * @param fullNameExact Precision of full name extraction using exact string matching.
  * @param fullNameEdit Normalized edit distance precision of full name.
  * @param lastNameExact Precision of last name extraction using exact string matching.
  * @param lastNameEdit Normalized edit distance precision of last name.
  */
case class CoreMetadataMatchStats(
  nEntries: Int,
  titleExact: Double, titleEdit: Double,
  fullNameCountDiff: Double, fullNameExact: Double, fullNameEdit: Double,
  lastNameExact: Double, lastNameEdit: Double,
  bibScore: Double
)

object CoreMetadataMatchStats {
  val formatter = new DecimalFormat("#.##")

  def printSummary(matchSummary: CoreMetadataMatchStats) {
    println(s"Scoring performance on ${matchSummary.nEntries} papers: ")

    println(s"Precision on titles exact/edit: ${
      formatter.format(matchSummary.titleExact)
    }%/${formatter.format(matchSummary.titleEdit)}%")

    println(s"Average difference in number of author full names: ${
      formatter.format(matchSummary
        .fullNameCountDiff)
    }")
    println(s"Precision on author full names exact/edit: ${
      formatter.format(matchSummary.fullNameExact)
    }%/ ${formatter.format(matchSummary.fullNameEdit)}%")

    println(s"Precision on author last names exact/edit: ${
      formatter.format(matchSummary.lastNameExact)
    }%/${formatter.format(matchSummary.lastNameEdit)}")

    println(s"Precision on bib extraction: ${formatter.format(matchSummary.bibScore)}%")
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
    authorLastNameEdit: Double,
    bibScore: Option[Double]
) {

  /** @param algoName The name of the algo such as Grobid, Metatagger, etc.
    * @return Return a row in Tableau format for this match.
    */
  def getValueRow(algoName: String): String = {
    import org.allenai.scholar.metrics.metadata.CoreMetadataMatch._

    def boolToInt(b: Boolean): Int = if (b) 1 else 0

    algoName + sep + fields.map { f =>
      f.setAccessible(true)
      if (f.getType.getName == "boolean") boolToInt(f.getBoolean(this)) else f.get(this)
    }.mkString(sep)
  }
}

object CoreMetadataMatch {
  val sep = "\t"

  lazy val fields = classOf[CoreMetadataMatch].getDeclaredFields

  /** @return Return the header in Tableau format.
    */
  def getHeaderRow: String = "algorithm" + sep + fields.map(_.getName).mkString(sep)

  def matchCoreMetadata(
    id: String,
    m1: CoreMetadata,
    m2: CoreMetadata,
    bibs: Option[Map[String, CoreMetadata]]
  ): CoreMetadataMatch = {
    import Parser.StringImplicits
    def editPrecision(s1: String, s2: String): Double = {
      val d = EditDistance.editDistance(s1, s2)
      val ed = if (d == 0) 0.0 else d * 1.0 / Seq(s1.length, s2.length).max
      1 - ed
    }

    def matchLists(l1: Seq[String], l2: Seq[String]): (Int, Boolean, Double) = {
      val countDiff = l1.size - l2.size
      val exact = countDiff == 0 && l1.zip(l2).forall(pair => pair._1 == pair._2)
      val edit = editPrecision(l1.mkString(";"), l2.mkString(";"))
      (countDiff, exact, edit)
    }

    val (fullNames1, fullNames2) = (m1.authorNames, m2.authorNames)
    val lastNames1 = fullNames1.map(_.lastNameFromFull)
    val lastNames2 = fullNames2.map(_.lastNameFromFull)

    val (fullNameCountDiff, fullNameExact, fullNameEdit) = matchLists(fullNames1, fullNames2)
    val (lastNameCountDiff, lastNameExact, lastNameEdit) = matchLists(lastNames1, lastNames2)

    val bibScore = bibs match {
      case Some(bibMap) =>
        val scores = for {
          b <- m1.bibs
          key = CoreMetadata.bibKey(b)
          score <- bibMap.get(key) match {
            case Some(cm) =>
              Some(editPrecision(b.title, cm.title))
            case None => None
          }
        } yield score

        Some(scores.sum / bibMap.size)
      case None => None
    }

    CoreMetadataMatch(
      id = id,
      titleExact = m1.title == m2.title,
      titleEdit = editPrecision(m1.title, m2.title),
      authorFullNameExact = fullNameExact,
      authorFullNameCountDiff = fullNameCountDiff,
      authorFullNameEdit = fullNameEdit,
      authorLastNameExact = lastNameExact,
      authorLastNameCountDiff = lastNameCountDiff,
      authorLastNameEdit = lastNameEdit,
      bibScore = bibScore
    )
  }

  def stats(matches: Seq[CoreMetadataMatch]): CoreMetadataMatchStats = {
    val nEntries = matches.size

    def percent(raw: Double): Double = raw * 100 / nEntries

    val titleExact = percent(matches.count(m => m.titleExact))
    val titleEdit = percent(matches.map(_.titleEdit).sum)

    val fullNameCountDiff = matches.map(_.authorFullNameCountDiff).sum * 1.0 / nEntries
    val fullNameExact = percent(matches.count(m => m.authorFullNameExact))
    val fullNameEdit = percent(matches.map(_.authorFullNameEdit).sum)

    val lastNameExact = percent(matches.count(m => m.authorLastNameExact))
    val lastNameEdit = percent(matches.map(_.authorLastNameEdit).sum)

    val bibScores = matches.flatMap(_.bibScore)
    val bibScore = bibScores.sum * 100 / bibScores.size

    CoreMetadataMatchStats(
      nEntries = nEntries,
      titleExact = titleExact, titleEdit = titleEdit,
      fullNameCountDiff = fullNameCountDiff,
      fullNameExact = fullNameExact, fullNameEdit = fullNameEdit,
      lastNameExact = lastNameExact, lastNameEdit = lastNameEdit,
      bibScore = bibScore
    )
  }
}

