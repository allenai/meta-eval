package org.allenai.scholar.metrics.metadata

import org.apache.commons.lang3.StringUtils._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source

case class PaperMetadata(
    id: String,
    title: String,
    venue: String,
    year: Int,
    authors: List[String]
) {
  @transient lazy val authorLastNames = authors map (_ dropWhile (_ == ' ') takeWhile (_ != ','))
}

object PaperMetadata {
  implicit val JsFormat = jsonFormat5(PaperMetadata.apply)

  def fromJsonLinesFile(metaFileName: String): Iterator[PaperMetadata] =
    Source.fromFile(metaFileName).getLines.map {
      _.parseJson.convertTo[PaperMetadata]
    }

  def convertToCore(meta: Iterator[PaperMetadata], stripAcc: Boolean = true): Map[String, CoreMetadata] = {
    def normalize(s: String) = {
      val res = s.trim.toLowerCase
      if (stripAcc) stripAccents(res) else res
    }

    meta.map { m =>
      m.id -> CoreMetadata(
        m.title.toLowerCase,
        m.authorLastNames.map(normalize).sorted.zip(m.authors.map(normalize).sorted)
      )
    }
  }.toMap
}

