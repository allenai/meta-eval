package org.allenai.scholar

import java.time.Year

import org.allenai.scholar.StringUtils._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

case class PaperMetadata(
    title: Title,
    venue: Venue,
    year: Year,
    authors: Seq[Author]
) {
}

object PaperMetadata {

  implicit val JsFormat =
    jsonFormat4((t: Title, v: Venue, y: Int, a: Seq[Author]) => PaperMetadata(t, v, Year.of(y), a))

  def fromJsonLinesFile(metaFileName: String): Map[String, PaperMetadata] = {
    case class Record(year: Int, id: String, authors: Seq[String], title: String, venue: String)
    implicit val format = jsonFormat5(Record)
    val metadataWithid = for (line <- Source.fromFile(metaFileName, "UTF-8").getLines) yield {
      val Record(year, id, authors, title, venue) = line.parseJson.convertTo[Record]
      (id, PaperMetadata(Title(title), Venue(venue), Year.of(year), authors.map(Author.parse)))
    }
    metadataWithid.toMap
  }
}

