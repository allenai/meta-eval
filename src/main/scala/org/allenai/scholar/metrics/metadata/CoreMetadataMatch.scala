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
  titleExact: Double, titleNonEmpty: Double, titleEdit: Double, titleEditPrecision: Double,
  fullNameCountDiff: Double, fullNameExact: Double, fullNameNonEmpty: Double, fullNameEdit: Double,
  fullNameEditPrecision: Double, lastNameExact: Double, lastNameNonEmpty: Double,
  lastNameEdit: Double, lastNameEditPrecision: Double,
  venueExact: Double, venueNonEmpty: Double, venueEdit: Double, venueEditPrecision: Double,
  yearExact: Double, yearNonZero: Double,
  bibScore: Double
)

object CoreMetadataMatchStats {
  val formatter = new DecimalFormat("#.##")

  def printSummary(matchSummary: CoreMetadataMatchStats) {
    // scalastyle:off
    println(s"Scoring performance on ${matchSummary.nEntries} papers: ")

    println(s"Exact precision on titles: ${formatter.format(matchSummary.titleExact)}%")
    println(s"Percentage of non-empty titles: ${formatter.format(matchSummary.titleNonEmpty)}%")
    println(s"Average edit distance on titles: ${formatter.format(matchSummary.titleEdit)}")
    println(s"Edit distance precision on titles: ${formatter.format(matchSummary.titleEditPrecision)}%")

    println(s"Average difference in number of author full names: ${formatter.format(matchSummary.fullNameCountDiff)}")
    println(s"Percentage non-empty author full names: ${formatter.format(matchSummary.fullNameNonEmpty)}%")
    println(s"Exact precision on author full names: ${formatter.format(matchSummary.fullNameExact)}%")
    println(s"Average edit distance on author full names: ${formatter.format(matchSummary.fullNameEdit)}")
    println(s"Edit distance precision on author full names: ${
      formatter.format(matchSummary.fullNameEditPrecision)
    }%")

    println(s"Exact precision on author last names: ${formatter.format(matchSummary.lastNameExact)}%")
    println(s"Percentage non-empty author last names: ${formatter.format(matchSummary.lastNameNonEmpty)}%")
    println(s"Average edit distance on author last names: ${formatter.format(matchSummary.lastNameEdit)}")
    println(s"Edit distance precision on author last names: ${
      formatter.format(matchSummary.lastNameEditPrecision)
    }%")

    println(s"Precision on bib extraction: ${formatter.format(matchSummary.bibScore)}%")
    // scalastyle:on
  }
}

case class CoreMetadataMatch(
    id: String,
    titleExact: Boolean,
    titleNonEmpty: Boolean,
    titleEdit: Double,
    authorFullNameExact: Boolean,
    authorFullNameNonEmpty: Boolean,
    authorFullNameCountDiff: Int,
    authorFullNameEdit: Double,
    authorLastNameExact: Boolean,
    authorLastNameNonEmpty: Boolean,
    authorLastNameCountDiff: Int,
    authorLastNameEdit: Double,
    venueExact: Boolean,
    venueNonEmpty: Boolean,
    venueEdit: Double,
    yearExact: Boolean,
    yearNonZero: Boolean,
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

  /** Compute CoreMetadataMatch
    * @param id of the ground-truth paper itself
    * @param predictedMeta the CoreMetadata (with bibs!) to be tested against ground truth
    * @param groundTruthMeta the ground truth metadata
    * @param groundTruthBibsByBibKey map of the ground truth bibliography, keyed by
    *                                CoreMetadata.edgesToBibKeyMap
    * @return overall stats of the match
    */
  def matchCoreMetadata(
    id: String,
    predictedMeta: CoreMetadata,
    groundTruthMeta: CoreMetadata,
    groundTruthBibsByBibKey: Option[Map[String, CoreMetadata]]
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

    val (pFullNames, gtFullNames) = (predictedMeta.authorNames, groundTruthMeta.authorNames)
    val pLastNames = pFullNames.map(_.lastNameFromFull)
    val gtLastNames = gtFullNames.map(_.lastNameFromFull)

    val (fullNameCountDiff, fullNameExact, fullNameEdit) = matchLists(pFullNames, gtFullNames)
    val (lastNameCountDiff, lastNameExact, lastNameEdit) = matchLists(pLastNames, gtLastNames)

    val bibScore = groundTruthBibsByBibKey match {
      case Some(groundTruthBibMap) =>
        val scores = for {
          predictedBib <- predictedMeta.bibs
          key = CoreMetadata.bibKey(predictedBib)
          score <- groundTruthBibMap.get(key) match {
            case Some(groundTruthBib) =>
              Some(editPrecision(predictedBib.title, groundTruthBib.title))
            case None => None
          }
        } yield score

        Some(scores.sum / groundTruthBibMap.size)
      case None => None
    }

    CoreMetadataMatch(
      id = id,
      titleExact = predictedMeta.title == groundTruthMeta.title,
      titleNonEmpty = predictedMeta.title.nonEmpty,
      titleEdit = editPrecision(predictedMeta.title, groundTruthMeta.title),
      authorFullNameExact = fullNameExact,
      authorFullNameNonEmpty = pLastNames.count(_.nonEmpty) > 0,
      authorFullNameCountDiff = fullNameCountDiff,
      authorFullNameEdit = fullNameEdit,
      authorLastNameExact = lastNameExact,
      authorLastNameNonEmpty = pLastNames.count(_.nonEmpty) > 0,
      authorLastNameCountDiff = lastNameCountDiff,
      authorLastNameEdit = lastNameEdit,
      venueExact = predictedMeta.venue == groundTruthMeta.venue,
      venueNonEmpty = predictedMeta.venue.nonEmpty,
      venueEdit = editPrecision(predictedMeta.venue, groundTruthMeta.venue),
      yearExact = predictedMeta.publishedYear == groundTruthMeta.publishedYear,
      yearNonZero = predictedMeta.publishedYear != yearZero,
      bibScore = bibScore
    )
  }

  def stats(matches: Seq[CoreMetadataMatch]): CoreMetadataMatchStats = {
    val nEntries = matches.size
    def editPrecision(edit: Double) = 100 - 100 * edit
    def percent(f: CoreMetadataMatch => Boolean): Double = matches.count(f) * 100.0 / nEntries

    val titleExact = percent(_.titleExact)
    val titleNonEmpty = percent(_.titleNonEmpty)
    val titleEdit = matches.map(_.titleEdit).sum / nEntries
    val titleEditPrecision = editPrecision(titleEdit)

    val fullNameCountDiff = matches.map(_.authorFullNameCountDiff).sum * 1.0 / nEntries
    val fullNameExact = percent(_.authorFullNameExact)
    val fullNameNonEmpty = percent(_.authorFullNameNonEmpty)
    val fullNameEdit = matches.map(_.authorFullNameEdit).sum / nEntries
    val fullNameEditPrecision = editPrecision(fullNameEdit)

    val lastNameExact = percent(_.authorLastNameExact)
    val lastNameNonEmpty = matches.count(_.authorLastNameNonEmpty) * 100.0 / nEntries
    val lastNameEdit = matches.map(_.authorLastNameEdit).sum / nEntries
    val lastNameEditPrecision = editPrecision(lastNameEdit)

    val venueExact = percent(_.venueExact)
    val venueNonEmpty = percent(_.venueNonEmpty)
    val venueEdit = matches.map(_.venueEdit).sum / nEntries
    val venueEditPrecision = editPrecision(venueEdit)

    val yearExact = percent(_.yearExact)
    val yearNonZero = percent(_.yearNonZero)

    val bibScores = matches.flatMap(_.bibScore)
    val bibScore = bibScores.sum * 100 / bibScores.size

    CoreMetadataMatchStats(
      nEntries = nEntries,
      titleExact = titleExact, titleEdit = titleEdit, titleEditPrecision = titleEditPrecision,
      titleNonEmpty = titleNonEmpty, fullNameCountDiff = fullNameCountDiff,
      fullNameExact = fullNameExact, fullNameNonEmpty = fullNameNonEmpty,
      fullNameEdit = fullNameEdit, fullNameEditPrecision = fullNameEditPrecision,
      lastNameExact = lastNameExact, lastNameNonEmpty = lastNameNonEmpty,
      lastNameEdit = lastNameEdit, lastNameEditPrecision = lastNameEditPrecision,
      venueExact = venueExact, venueNonEmpty = venueNonEmpty, venueEdit = venueEdit,
      venueEditPrecision = venueEditPrecision,
      yearExact = yearExact, yearNonZero = yearNonZero,
      bibScore = bibScore
    )
  }
}

