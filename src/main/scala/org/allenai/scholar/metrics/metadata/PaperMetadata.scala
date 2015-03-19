package org.allenai.scholar.metrics.metadata

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

import java.time.Year

case class PaperMetadata(
  id: String,
  title: String,
  venue: String,
  year: Int,
  authors: List[String]
)

object PaperMetadata {
  implicit val JsFormat = jsonFormat5(PaperMetadata.apply)

  def fromJsonLinesFile(metaFileName: String): Iterator[PaperMetadata] =
    Source.fromFile(metaFileName).getLines.map {
      _.parseJson.convertTo[PaperMetadata]
    }

  def convertToCore(meta: Iterator[PaperMetadata]): Map[String, CoreMetadata] = {
    import Parser.StringImplicits
    meta.map { m =>
      m.id -> CoreMetadata(
        title = m.title.toLowerCase,
        authorNames = m.authors.map(_.normalize).sorted,
        venue = m.venue,
        publishedYear = Year.parse(m.year.toString)
      )
    }
  }.toMap
}

