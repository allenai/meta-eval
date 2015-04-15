package org.allenai.scholar.metrics.metadata

import java.time.Year

import org.allenai.scholar.{ Author, Title, Venue }
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

case class PaperMetadata(
  title: Title,
  venue: Venue,
  year: Year,
  authors: Seq[Author]
)

object PaperMetadata {

  implicit val yearFmt = new JsonFormat[Year] {
    override def write(obj: Year): JsValue = JsNumber.apply(obj.getValue)

    override def read(json: JsValue): Year = json match {
      case num: JsNumber => Year.of(num.value.toInt)
      case _ => deserializationError("Year should be formatted as an integer")
    }
  }

  implicit val JsFormat = jsonFormat4(PaperMetadata.apply)

  def fromJsonLinesFile(metaFileName: String): Map[String, PaperMetadata] = {
    case class Record(year: Int, id: String, authors: Seq[String], title: String, venue: String)
    implicit val format = jsonFormat5(Record)
    val metadataWithid = for (line <- Source.fromFile(metaFileName, "UTF-8").getLines) yield {
      val Record(year, id, authors, title, venue) = line.parseJson.convertTo[Record]
      (id, fromStrings(year, authors, title, venue))
    }
    metadataWithid.toMap
  }

  def fromStrings(
    year: Int,
    authors: Seq[String], title: String, venue: String
  ): PaperMetadata = {
    PaperMetadata(Title(title), Venue(venue), Year.of(year), authors.map(Author.parse))
  }
}

