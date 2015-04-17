package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar.metrics.{ ErrorAnalysis, PaperMetadata }
import org.allenai.scholar.{ Author, Title }

object BibliographyErrorAnalysis {
  def computeMetrics(
    truth: Seq[(String, Set[PaperMetadata])],
    predictions: Map[String, Set[PaperMetadata]]
  ) =
    ErrorAnalysis.computeMetrics(truth, predictions,
      BIBLIOGRAPHY_EXTRACTION -> extractBibliography _)

  val BIBLIOGRAPHY_EXTRACTION = "bibliography"

  def extractBibliography(bibs: Set[PaperMetadata]): Iterable[FuzzyPaperMetadata] =
    bibs.map(fuzzify)

  // Soft version of paper metadata:
  def fuzzify(data: PaperMetadata) =
    FuzzyPaperMetadata(
      Title(data.title.normalized.text),
      data.year,
      data.authors.map(_.lastNameOnly.normalized).toSet
    )

  case class FuzzyPaperMetadata(title: Title, year: Year, authors: Set[Author]) {
    override def equals(a: Any) = a match {
      case FuzzyPaperMetadata(title2, year2, authors2) =>
        title == title2 ||
          (authors == authors2 && year == year2)
      case _ => false
    }
    override def hashCode = 1
  }

}
