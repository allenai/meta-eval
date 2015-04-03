package org.allenai.scholar.metrics.metadata

import java.io.File

import org.allenai.scholar.{ MetadataAndBibliography, Venue, Author, Title }
import org.apache.commons.lang3.StringUtils._
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import org.jsoup.select.Elements
import java.time.Year

import scala.io.Source

/** A collection of text processing, normalization, and
  * Jsoup-based utilities packaged in implicits to reduce boiler plate.
  */

import org.allenai.scholar.StringUtils._
import org.allenai.scholar.metrics.metadata.Parser.ElementsImplicit.elementsToSeq
import org.allenai.scholar.metrics.metadata.Parser.JsoupElementsImplicits

abstract class Parser(
    titlePath: String,
    authorPath: String,
    lastRelativePath: String,
    firstRelativePath: String,
    middleRelativePath: String,
    bibMainPath: String,
    bibAuthorPath: String,
    bibTitlePath: String
) {

  def extractBibYear(bib: Element): Year

  def extractVenue(bib: Element): String

  def extractSpecialBib(bib: Element): PaperMetadata

  protected def extractNames(e: Element, authorPath: String, initial: Boolean = false) =
    e.select(authorPath).map(a => {
      val last = a.extractName(lastRelativePath)
      val first = a.extractName(firstRelativePath)
      val middle = a.extractName(middleRelativePath)
      Author(first, if (middle.size > 0) List(middle) else List(), last)
    }).map(_.ifDefined).flatten.toList

  private def extractBibs(doc: Document): Seq[PaperMetadata] =
    doc.select(bibMainPath).map(bib =>
      bib.extractBibTitle(bibTitlePath) match {
        case title if title.nonEmpty =>
          PaperMetadata(
            title = Title(title),
            authors = extractNames(bib, bibAuthorPath),
            venue = Venue(extractVenue(bib)),
            year = extractBibYear(bib)
          )
        case _ => extractSpecialBib(bib)
      }).toList

  def parseCoreMetadata(file: File): Option[MetadataAndBibliography] = try {
    val xmlString = Source.fromFile(file, "UTF-8").getLines().mkString("\n")
    Some(parseCoreMetadataString(xmlString))
  } catch {
    case e: Exception =>
      println(s"Could not parse xml file ${file.getName}")
      e.printStackTrace()
      None
  }

  /** Function that parses XML to produce core metadata.
    * Names are lower-cased and trimmed of non-letter at the end.
    * @param xmlString The XML data as a string
    * @return The paper's core metadata.
    */
  def parseCoreMetadataString(xmlString: String): MetadataAndBibliography = {
    val doc = Jsoup.parse(xmlString)
    val metadata = PaperMetadata(
      title = Title(doc.extractTitle(titlePath)),
      authors = extractNames(doc, authorPath),
      year = org.allenai.scholar.metrics.metadata.yearZero,
      venue = Venue("")
    )
    MetadataAndBibliography(
      metadata = metadata,
      bibs = extractBibs(doc)
    )
  }
}

object GrobidParser extends Parser(
  titlePath = "teiHeader>fileDesc>titleStmt>title",
  authorPath = "teiHeader>fileDesc>sourceDesc>biblStruct>analytic>author",
  lastRelativePath = "persName>surname",
  firstRelativePath = "persName>forename[type=first]",
  middleRelativePath = "persName>forename[type=middle]",
  bibMainPath = "listBibl>biblStruct",
  bibAuthorPath = "analytic>author",
  bibTitlePath = "analytic>title[type=main]"
) {

  def extractBibYear(bib: Element): Year =
    bib.extractYear("monogr>imprint>date[type=published]", _.attr("when"))

  def extractVenue(bib: Element): String = bib.extractBibTitle("monogr>title")

  def extractSpecialBib(bib: Element): PaperMetadata =
    PaperMetadata(
      title = Title(extractVenue(bib)), // venue becomes title for PhD theses
      authors = extractNames(bib, "monogr>author"),
      venue = Venue(""),
      year = extractBibYear(bib)
    )
}

object MetataggerParser extends Parser(
  titlePath = "document>content>headers>title",
  authorPath = "document>content>headers>authors>author",
  lastRelativePath = "author-last",
  firstRelativePath = "author-first",
  middleRelativePath = "author-middle",
  bibMainPath = "biblio>reference",
  bibAuthorPath = "authors>author",
  bibTitlePath = "title"
) {

  def extractBibYear(bib: Element): Year = bib.extractYear("date", _.text)

  def extractVenue(bib: Element): String =
    List("conference", "journal", "booktitle")
      .find(vt => !bib.select(vt).isEmpty) match {
        case Some(v) => bib.extractBibTitle(v)
        case None => ""
      }

  def extractSpecialBib(bib: Element): PaperMetadata = {
    val metadata = PaperMetadata(
      title = Title(bib.extractBibTitle("booktitle")),
      authors = extractNames(bib, "authors>author"),
      venue = Venue(""),
      year = extractBibYear(bib)
    )
    metadata
  }
}

object Parser {

  import org.allenai.scholar.StringUtils._

  object ElementsImplicit {

    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    /** Convenient implicit: no need for .asScala any time calling Jsoup's select. */
    implicit def elementsToSeq(elms: Elements): collection.mutable.Buffer[Element] = elms.asScala
  }

  implicit class JsoupElementsImplicits(e: Element) {

    import ElementsImplicit._

    def extract(path: String): String = e.select(path).headOption match {
      case Some(v) => v.text
      case None => ""
    }

    def extractName(namePath: String): String =
      extract(namePath).trimNonAlphabetic()

    def extractTitle(path: String): String = extract(path)

    def extractBibTitle(path: String): String =
      e.extract(path).trimChars(",.")

    def extractYear(path: String, get: Element => String): Year =
      e.select(path).headOption match {
        case Some(d) => get(d).extractYear
        case None => yearZero
      }

  }

}
