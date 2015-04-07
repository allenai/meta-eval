package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar.metrics.ErrorAnalysis
import org.allenai.scholar._

object MetadataErrorAnalysis {
  def computeMetrics(
    truth: Map[String, PaperMetadata],
    predictions: Map[String, PaperMetadata]
  ) =
    ErrorAnalysis.computeMetrics(truth, predictions,
      YEAR -> extractYear,
      VENUE_NONEMPTY -> extractVenueNonEmpty,
      AUTHOR_FULL_NAME -> extractAuthorExact,
      AUTHOR_NORMALIZED_LAST_NAME -> extractAuthorLastName,
      AUTHOR_NORMALIZED_LAST_NAME_FIRST_INITIAL -> extractAuthorLastNameFirstInitial,
      TITLE_EXACT -> extractTitleExact,
      TITLE_NORMALIZED -> extractTitleNormalized,
      TITLE_NONEMPTY -> extractTitleNonempty)

  def extractYear(data: PaperMetadata): Iterable[Year] = PublicationYear.ifDefined(data.year)

  def extractVenueNonEmpty(data: PaperMetadata): Iterable[Venue] = data.venue.nonEmpty.ifDefined

  def extractAuthorExact(data: PaperMetadata): Iterable[Author] = data.authors

  def extractAuthorLastName(data: PaperMetadata): Iterable[Author] =
    data.authors.map(_.lastNameOnly.normalized)

  def extractAuthorLastNameFirstInitial(data: PaperMetadata): Iterable[Author] =
    data.authors.map(_.lastNameFirstInitial.normalized)

  def extractTitleExact(data: PaperMetadata): Iterable[Title] = data.title.ifDefined

  def extractTitleNormalized(data: PaperMetadata): Iterable[Title] = data.title.normalized.ifDefined

  def extractTitleNonempty(data: PaperMetadata): Iterable[Title] = data.title.nonEmpty.ifDefined

  private val YEAR = "year"
  private val AUTHOR_FULL_NAME = "authorFullName"
  private val AUTHOR_NORMALIZED_LAST_NAME = "authorLastNameNormalized"
  private val AUTHOR_NORMALIZED_LAST_NAME_FIRST_INITIAL = "authorLastNameNormalizedFirstInitial"
  private val TITLE_EXACT = "titleExact"
  private val TITLE_NORMALIZED = "titleNormalized"
  private val TITLE_NONEMPTY = "titleNonEmpty"
  private val VENUE_NONEMPTY = "venueNonEmpty"

}
