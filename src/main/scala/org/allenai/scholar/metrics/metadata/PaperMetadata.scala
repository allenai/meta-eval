package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar._
import StringUtils._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

case class PaperMetadata(
    title: Title,
    venue: Venue,
    year: Year,
    authors: Seq[Author]) {
  // Soft version of paper metadata:
  // includes just the first word of the title and the last names of the first two authors
  def fuzzy: PaperMetadata =
    PaperMetadata(
      Title(title.normalized.text.splitOnWhitespace.head),
      Venue(""),
      year,
      authors.map(_.lastNameOnly).take(2)
    )
}

object PaperMetadata {

  import Title._
  import Author._

  implicit val JsFormat =
    jsonFormat4((t: Title, v: Venue, y: Int, a: Seq[Author]) => PaperMetadata(t, v, Year.of(y), a))

  def fromJsonLinesFile(metaFileName: String): Map[String, PaperMetadata] =
    Source.fromFile(metaFileName, "UTF-8").getLines.map {
      _.parseJson.convertTo[(String, PaperMetadata)]
    }.toMap
}

