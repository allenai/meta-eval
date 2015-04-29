package org.allenai.scholar.metrics.metadata

import java.io.File
import java.time.Year

import org.allenai.scholar._
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import org.jsoup.select.Elements

import scala.io.Source
import scala.util.{ Failure, Try }

/** A collection of text processing, normalization, and
  * Jsoup-based utilities packaged in implicits to reduce boiler plate.
  */

object GrobidParser {

  import Implicits._
  import StringUtils._

  def year(bib: Element): Year =
    bib.findAttributeValue("monogr>imprint>date[type=published]", "when").extractYear

  def author(e: Element): Option[Author] = Author(
    lastName = e.findText("persName>surname").toName,
    firstName = e.findText("persName>forename[type=first]").toName,
    middleNames = e.select("persName>forename[type=middle]").map(_.text.toName)
  ).ifDefined

  def extractMetadataAndBib(xmlString: String): MetadataAndBibliography = {
    val doc = extractStructuredDocument(xmlString)
    MetadataAndBibliography(metadata = doc.metadata, bibs = doc.bibliography.entries.map(_._1))
  }

  def extractStructuredDocument(xmlString: String): StructuredDocument = {
    val doc = Jsoup.parse(xmlString, "", org.jsoup.parser.Parser.xmlParser())
    val metadata = PaperMetadata(
      title =
        Title(doc.findText("teiHeader>fileDesc>titleStmt>title")),
      authors =
        doc.select("teiHeader>fileDesc>sourceDesc>biblStruct>analytic>author").flatMap(author),
      year =
        doc.findAttributeValue("teiHeader>fileDesc>sourceDesc>biblStruct>monogr>imprint>date[type=published]", "when").extractYear,
      venue =
        Venue(doc.findText("teiHeader>fileDesc>sourceDesc>biblStruct>monogr>title").toTitle)
    )
    val bibs = for {
      bib <- doc.select("listBibl>biblStruct")
      title = bib.findText("analytic>title[type=main]").toTitle
    } yield {
      val id = bib.attr("xml:id")
      if (title.nonEmpty) {
        (id, PaperMetadata(
          title = Title(title),
          authors = bib.select("analytic>author").flatMap(author).toList,
          venue = Venue(bib.findText("monogr>title")),
          year = year(bib)
        ))
      } else {
        // For Ph.D. dissertations, title is journal name
        (id, PaperMetadata(
          title = Title(bib.findText("monogr>title")),
          venue = Venue(""),
          authors = bib.select("monogr>author").flatMap(author).toList,
          year = year(bib)
        ))
      }
    }
    StructuredDocument(
      metadata = metadata,
      paperAbstract = None,
      body = Vector(),
      bibliography = Bibliography(bibs.map(t => (t._2, List())).toIndexedSeq)
    )
  }
  object Implicits {

    import org.allenai.scholar.StringUtils._

    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    /** Convenient implicit: no need for .asScala any time calling Jsoup's select. */
    implicit def elementsToSeq(elms: Elements): collection.mutable.Buffer[Element] = elms.asScala

    implicit class Normlizations(s: String) {
      def toName = s.trimNonAlphabetic()
      def toTitle = {
        s.trimChars(",.").find(c => Character.isAlphabetic(c)) match {
          case None => ""
          case Some(_) => s
        }
      }
    }

    implicit class JsoupElementsImplicits(e: Element) {

      def findText(path: String): String =
        e.select(path).headOption.map(_.text).getOrElse("")

      def findAttributeValue(path: String, attrName: String): String =
        e.select(path).headOption.map(_.attr(attrName)).getOrElse("")

    }

  }

}

