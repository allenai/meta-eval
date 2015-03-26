package org.allenai.scholar.metrics.metadata

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

import java.time.Year
import java.time.format.DateTimeParseException

case class PaperMetadata(
    id: String,
    title: String,
    venue: String,
    year: Int,
    authors: List[String]
) {
  import Parser.StringImplicits
  def toCore: (String, CoreMetadata) = {
    id -> CoreMetadata(
      title = title.toLowerCase,
      authorNames = authors.map(_.normalize),
      venue = venue,
      publishedYear = yearIntAsYear
    )
  }
  def authorLastNames = authors.map(_.lastNameFromFull)
  def yearIntAsYear: Year = {
    if (year == 0) {
      yearZero
    } else {
      try {
        Year.parse(year.toString)
      } catch {
        case e: DateTimeParseException => yearZero
      }
    }
  }
}

object PaperMetadata {
  implicit val JsFormat = jsonFormat5(PaperMetadata.apply)

  def fromJsonLinesFile(metaFileName: String): Iterator[PaperMetadata] =
    Source.fromFile(metaFileName).getLines.map {
      _.parseJson.convertTo[PaperMetadata]
    }

  def convertToCore(pm: Iterator[PaperMetadata]): Map[String, CoreMetadata] =
    pm.map(_.toCore).toMap
}

