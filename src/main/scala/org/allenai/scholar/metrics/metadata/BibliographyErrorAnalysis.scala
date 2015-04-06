package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.{ Venue, Title, PaperMetadata }
import org.allenai.scholar.metrics.ErrorAnalysis
import org.allenai.scholar.StringUtils._

object BibliographyErrorAnalysis {
  def computeMetrics(
    truth: Map[String, Set[PaperMetadata]],
    predictions: Map[String, Set[PaperMetadata]]
  ) =
    ErrorAnalysis.computeMetrics(truth, predictions,
      BIBLIOGRAPHY_EXTRACTION -> extractBibliography _)

  val BIBLIOGRAPHY_EXTRACTION = "bibliography"

  def extractBibliography(bibs: Set[PaperMetadata]): Iterable[PaperMetadata] =
    bibs.map(fuzzify)

  // Soft version of paper metadata:
  // includes just the first word of the title and the last names of the first two authors
  def fuzzify(data: PaperMetadata) =
    PaperMetadata(
      Title(data.title.normalized.text.splitOnWhitespace.head),
      Venue(""),
      data.year,
      data.authors.map(_.lastNameOnly.normalized).take(2)
    )

}
