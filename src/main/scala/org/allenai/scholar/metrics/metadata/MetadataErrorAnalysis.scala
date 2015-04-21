package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar.metrics.ErrorAnalysis
import org.allenai.scholar._

object MetadataErrorAnalysis {
  def computeMetrics(
    truth: Seq[(String, PaperMetadata)],
    predictions: Map[String, PaperMetadata]
  ) =
    ErrorAnalysis.computeMetrics(truth, predictions,
      YEAR -> extractYearExact _,
      YEAR_NONEMPTY -> extractYearNonEmpty _,
      VENUE_NONEMPTY -> extractVenueNonEmpty _,
      VENUE_NORMALIZED -> extractVenueNormalized _,
      VENUE_EXACT -> extractVenueExact _,
      AUTHOR_FULL_NAME -> extractAuthorExact _,
      AUTHOR_NORMALIZED_LAST_NAME -> extractAuthorLastName _,
      AUTHOR_NORMALIZED_LAST_NAME_FIRST_INITIAL -> extractAuthorLastNameFirstInitial _,
      TITLE_EXACT -> extractTitleExact _,
      TITLE_NORMALIZED -> extractTitleNormalized _,
      TITLE_NONEMPTY -> extractTitleNonempty _)

  def extractYearExact(data: PaperMetadata): Iterable[Year] = PublicationYear.ifDefined(data.year)

  def extractYearNonEmpty(data: PaperMetadata): Iterable[Year] =
    PublicationYear.ifDefined(data.year).map(_ => Year.of(1)) // fixed constant

  def extractVenueNonEmpty(data: PaperMetadata): Iterable[Venue] = data.venue.nonEmpty.ifDefined

  def extractVenueNormalized(data: PaperMetadata): Iterable[Venue] =
    data.venue.ifDefined.map(_.normalized)

  def extractVenueExact(data: PaperMetadata): Iterable[Venue] = data.venue.ifDefined

  def extractAuthorExact(data: PaperMetadata): Iterable[Author] = data.authors

  def extractAuthorLastName(data: PaperMetadata): Iterable[Author] =
    data.authors.map(_.lastNameOnly.normalized)

  def extractAuthorLastNameFirstInitial(data: PaperMetadata): Iterable[Author] =
    data.authors.map(_.lastNameFirstInitial.normalized)

  def extractTitleExact(data: PaperMetadata): Iterable[Title] = data.title.ifDefined

  def extractTitleNormalized(data: PaperMetadata): Iterable[Title] = data.title.normalized.ifDefined

  def extractTitleNonempty(data: PaperMetadata): Iterable[Title] = data.title.nonEmpty.ifDefined

  private val AUTHOR_FULL_NAME = "authorFullName"
  private val AUTHOR_NORMALIZED_LAST_NAME = "authorLastNameNormalized"
  private val AUTHOR_NORMALIZED_LAST_NAME_FIRST_INITIAL = "authorLastNameNormalizedFirstInitial"
  private val TITLE_EXACT = "titleExact"
  private val TITLE_NORMALIZED = "titleNormalized"
  private val TITLE_NONEMPTY = "titleNonEmpty"
  private val VENUE_EXACT = "venueExact"
  private val VENUE_NONEMPTY = "venueNonEmpty"
  private val VENUE_NORMALIZED = "venueNormalized"
  private val YEAR = "year"
  private val YEAR_NONEMPTY = "yearNonEmpty"

}
