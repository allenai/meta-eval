package org.allenai.scholar.metrics.metadata

import java.io.File
import java.time.Year

import org.allenai.scholar._
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import org.jsoup.select.Elements

import scala.collection.JavaConverters
import scala.io.Source
import scala.util.{ Failure, Try }

/** A collection of text processing, normalization, and
  * Jsoup-based utilities packaged in implicits to reduce boiler plate.
  */

object GrobidParser {

  import Implicits._
  import StringUtils._
  import JavaConverters._

  def year(bib: Element): Year =
    bib.findAttributeValue("monogr>imprint>date[type=published]", "when").extractYear

  def author(e: Element): Option[Author] = Author(
    lastName = e.findText("persName>surname").toName,
    firstName = e.findText("persName>forename[type=first]").toName,
    middleNames = e.select("persName>forename[type=middle]").map(_.text.toName)
  ).ifDefined

  def extractMetadataAndBib(xmlString: String): MetadataAndBibliography = {
    val doc = Jsoup.parse(xmlString, "", org.jsoup.parser.Parser.xmlParser())
    MetadataAndBibliography(metadata = extractMetadata(doc), bibs = extractBibEntriesWithId(doc).map(_._2))
  }

  private def extractMetadata(doc: Element) = PaperMetadata(
    title =
      Title(doc.findText("teiHeader>fileDesc>titleStmt>title")),
    authors =
      doc.select("teiHeader>fileDesc>sourceDesc>biblStruct>analytic>author").flatMap(author),
    year =
      doc.findAttributeValue("teiHeader>fileDesc>sourceDesc>biblStruct>monogr>imprint>date[type=published]", "when").extractYear,
    venue =
      Venue(doc.findText("teiHeader>fileDesc>sourceDesc>biblStruct>monogr>title").toTitle)
  )

  private def extractBibEntriesWithId(doc: Element) =
    for {
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

  def extractStructuredDocument(xmlString: String): StructuredDocument = {
    val doc = Jsoup.parse(xmlString, "", org.jsoup.parser.Parser.xmlParser())
    val metadata = extractMetadata(doc)
    val bibs = extractBibEntriesWithId(doc)
    val paperAbstract = doc.select("teiHeader>profileDesc>abstract").headOption.map(_.text)
    val sections = for (div <- doc.select("text>body>div")) yield {
      val head = div.select("head").headOption
      val id = head match {
        case Some(h) => h.attr("n").ifNonEmpty
        case None => None
      }
      val text = head match {
        case Some(h) => div.children.filter(_ != h).map(_.text).mkString("")
        case None => div.text
      }
      Section(id = id, text = text, header = head.map(_.text))
    }
    val bibMentions = for (ref <- doc.select("ref[type=bibr")) yield {
      val id = ref.attr("target").dropWhile(_ == '#')
      val parentText = ref.parent.text
      val myText = ref.text
      val begin = parentText.indexOf(myText)
      val end = begin + myText.size
      (id, Mention(parentText, begin, end))
    }
    val mentionsOfBibEntry = bibMentions.toVector.groupBy(_._1).mapValues(_.map(_._2))
    val bibsWithMentions = for ((id, bib) <- bibs) yield {
      (bib, mentionsOfBibEntry.getOrElse(id, List()))
    }
    StructuredDocument(
      metadata = metadata,
      paperAbstract = paperAbstract,
      body = sections.toIndexedSeq,
      bibliography = Bibliography(bibsWithMentions.toIndexedSeq)
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
      def ifNonEmpty = if (s.nonEmpty) Some(s) else None
    }

    implicit class JsoupElementsImplicits(e: Element) {

      def findText(path: String): String =
        e.select(path).headOption.map(_.text).getOrElse("")

      def findAttributeValue(path: String, attrName: String): String =
        e.select(path).headOption.map(_.attr(attrName)).getOrElse("")

    }

  }

}

