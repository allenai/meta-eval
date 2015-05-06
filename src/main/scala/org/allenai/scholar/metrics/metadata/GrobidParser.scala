package org.allenai.scholar.metrics.metadata

import java.io.File
import java.time.Year

import org.allenai.scholar._
import org.jsoup.Jsoup
import org.jsoup.nodes.{ TextNode, Document, Element }
import org.jsoup.select.Elements

import scala.collection.JavaConverters
import scala.io.Source
import scala.util.{ Failure, Try }

/** Parser for Grobid XML documents
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
    } yield {
      val id = bib.attr("xml:id")
      val title = bib.findText("analytic>title[type=main]").toTitle match {
        case "" => Title(bib.findText("monogr>title"))
        case s => Title(s)
      }
      val authors = bib.select("analytic>author").flatMap(author).toList match {
        case List() => bib.select("monogr>author").flatMap(author).toList
        case l => l
      }
      val venue = Venue(bib.findText("monogr>title"))
      val yr = year(bib)
      (id, PaperMetadata(title = title, authors = authors, venue = venue, year = yr))
    }

  private def extractSectionInfo(div: Element) = {
    val bodyPlusHeaderText = div.text

    val head = div.select("head").headOption
    val (id, headerText, bodyTextOffset) = head match {
      case Some(h) =>
        val hText = h.text
        (
          h.attr("n").ifNonEmpty,
          Some(hText),
          hText.size + bodyPlusHeaderText.drop(hText.size).takeWhile(_ <= ' ').size
        )
      case None =>
        (None, None, 0)
    }
    val section = Section(id = id, text = bodyPlusHeaderText.drop(bodyTextOffset), header = head.map(_.text))
    (div, bodyPlusHeaderText, bodyTextOffset, section)
  }

  def extractStructuredDocument(xmlString: String): StructuredDocument = {
    val doc = Jsoup.parse(xmlString, "", org.jsoup.parser.Parser.xmlParser())
    val metadata = extractMetadata(doc)
    val bibs = extractBibEntriesWithId(doc)
    val paperAbstract = doc.select("teiHeader>profileDesc>abstract").headOption.map(_.text)
    val sectionInfo = doc.select("text>body>div").map(extractSectionInfo)
    val bibMentions =
      for {
        ref <- doc.select("ref[type=bibr")
        ((div, fullText, offset, _), sectionNumber) <- sectionInfo.zipWithIndex.find {
          case ((div, fullText, offset, _), i) => ref.parents.contains(div)
        }
      } yield {
        val id = ref.attr("target").dropWhile(_ == '#')
        val begin = ref.textOffset(div) - offset
        val end = begin + ref.text.size
        (id, Mention(sectionNumber, begin, end))
      }
    val mentionsOfBibEntry = bibMentions.toVector.groupBy(_._1).mapValues(_.map(_._2))
    val bibsWithMentions = for ((id, bib) <- bibs) yield {
      (bib, mentionsOfBibEntry.getOrElse(id, List()))
    }
    val footnotes = doc.select("text>body>note[place=foot]").map(_.text)
    StructuredDocument(
      metadata = metadata,
      paperAbstract = paperAbstract,
      sections = sectionInfo.map(_._4).toIndexedSeq,
      bibliography = Bibliography(bibsWithMentions.toIndexedSeq),
      footnotes = footnotes.toIndexedSeq
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

      // The number of text characters in the ancestor that preceed the given element
      def textOffset(ancestor: Element): Int = {
        if (ancestor == e.parent) {
          val ancestorText = ancestor.text
          val elementText = e.text
          val index = ancestorText.indexOf(elementText)
          ancestorText.indexOf(elementText, index + 1) match {
            case -1 => // The common and easy case: Text only occurs once in the parent.
              index
            case _ => // Our text occurs multiple times in the parent.  Bogus!
              // Count how many times it occurs previous to our element
              def countOccurencesIn(base: String) = {
                var count = 0
                var index = base.indexOf(elementText)
                while (index > 0) {
                  count += 1
                  index = base.indexOf(elementText, index + 1)
                }
                count
              }
              val precedingSiblingText =
                ancestor.childNodes.asScala.takeWhile(_ != e).map {
                  case t: TextNode => t.getWholeText.trim()
                  case e: Element => e.text
                  case _ => ""
                }
              val precedingCount = precedingSiblingText.map(countOccurencesIn).sum
              // Now get the next occurrence of our text
              def nthIndexOf(base: String, n: Int) = {
                var i = 0
                var index = base.indexOf(elementText)
                while (i < n) {
                  index = base.indexOf(elementText, index + 1)
                  i += 1
                }
                index
              }
              nthIndexOf(ancestorText, precedingCount)
          }
        } else if (e.parent == null) {
          sys.error("Must specify an ancestor element to find text offset")
        } else {
          e.parent.textOffset(ancestor) + e.textOffset(e.parent)
        }
      }

    }

  }

}

