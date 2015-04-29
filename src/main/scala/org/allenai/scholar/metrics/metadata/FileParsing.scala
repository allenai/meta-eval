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

trait FileParsing {

  def parseFile[T](file: File)(parse: String => T): Try[T] =
    Try {
      val xmlString = Source.fromFile(file, "UTF-8").getLines().mkString("\n")
      parse(xmlString)
    }

  def parseDir[T](
    dir: File,
    idFilter: String => Boolean,
    errorHandler: (File, Throwable) => Unit = {
      (f, ex) => println(s"Error parsing $f: ${ex.getMessage}")
    }
  )(parse: String => T): Map[String, T] =
    (for {
      f <- dir.listFiles
      id = f.getName.split('.')(0)
      if idFilter(id)
      predicted <- parseFile(f)(parse).recoverWith {
        case ex =>
          errorHandler(f, ex)
          Failure(ex)
      }.toOption
    } yield (id, predicted)).toMap
}

object GrobidParser {

  import FileParsing._
  import StringUtils._
  import ElementsImplicit._

  import org.allenai.scholar.metrics.metadata.FileParsing.ElementsImplicit._

  def year(bib: Element): Year =
    bib.findAttributeValue("monogr>imprint>date[type=published]", "when").extractYear

  def author(e: Element): Option[Author] = Author(
    lastName = e.findText("persName>surname"),
    firstName = e.findText("persName>forename[type=first]"),
    middleNames = e.select("persName>forename[type=middle]").map(_.text)
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
        Venue(doc.findText("teiHeader>fileDesc>sourceDesc>biblStruct>monogr>title").asTitle)
    )
    val bibs = for {
      bib <- doc.select("listBibl>biblStruct")
      title = bib.findText("analytic>title[type=main]").asTitle
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
}

//object MetataggerParser extends Parser(
//  titlePath = "document>content>headers>title",
//  venuePath = "???",
//  yearPath = "???",
//  authorPath = "document>content>headers>authors>author",
//  lastRelativePath = "author-last",
//  firstRelativePath = "author-first",
//  middleRelativePath = "author-middle",
//  bibMainPath = "biblio>reference",
//  bibAuthorPath = "authors>author",
//  bibTitlePath = "title"
//) {
//
//  override def extractBibYear(bib: Element): Year = bib.extractYear("date", _.text)
//
//  // TODO/NotYetImplemented
//  override def extractHeaderYear(elmt: Element): Year = yearZero
//
//  override def extractVenue(bib: Element): String =
//    List("conference", "journal", "booktitle")
//        .find(vt => !bib.select(vt).isEmpty) match {
//      case Some(v) => bib.extractBibTitle(v)
//      case None => ""
//    }
//
//  override def extractSpecialBib(bib: Element): PaperMetadata = {
//    val metadata = PaperMetadata(
//      title = Title(bib.extractBibTitle("booktitle")),
//      authors = extractNames(bib, "authors>author"),
//      venue = Venue(""),
//      year = extractBibYear(bib)
//    )
//    metadata
//  }
//}

object FileParsing {

  import org.allenai.scholar.StringUtils._

  object ElementsImplicit {

    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    /** Convenient implicit: no need for .asScala any time calling Jsoup's select. */
    implicit def elementsToSeq(elms: Elements): collection.mutable.Buffer[Element] = elms.asScala
  }

  implicit class Normlizations(s: String) {
    def toName = s.trimNonAlphabetic()
    def asTitle = {
      s.trimChars(",.").find(c => Character.isAlphabetic(c)) match {
        case None => ""
        case Some(_) => s
      }
    }
  }

  implicit class JsoupElementsImplicits(e: Element) {

    import ElementsImplicit._

    def findText(path: String): String =
      e.select(path).headOption.map(_.text).getOrElse("")

    def findAttributeValue(path: String, attrName: String): String =
      e.select(path).headOption.map(_.attr(attrName)).getOrElse("")

    def extractAuthors(
      authorPath: String,
      lastRelativePath: String,
      firstRelativePath: String,
      middleRelativePath: String
    ): List[Author] = {
      e.select(authorPath).map(a => {
        val last = a.findText(lastRelativePath).toName
        val first = a.findText(firstRelativePath).toName
        val middle = a.findText(middleRelativePath).toName
        Author(first, if (middle.nonEmpty) List(middle) else List(), last)
      }).flatMap(_.ifDefined).toList

    }
  }

}
