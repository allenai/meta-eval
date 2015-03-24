package org.allenai.scholar.metrics.metadata

import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }

import scala.io.Source

import java.io.File
import java.time.Year

/** Research papers' core metadata.
  * @param title The paper's title.
  * @param authorNames List of authors' full name, e.g. "Doe, John A."
  * @param publishedYear Year of publication.
  * @param venue The venue where the paper was published.
  * @param bibs A list of bibs/references in the paper.
  */
case class CoreMetadata(
  title: String,
  authorNames: List[String],
  publishedYear: Year = yearZero,
  venue: String = "",
  bibs: List[CoreMetadata] = List()
)

object CoreMetadata {

  import org.allenai.scholar.metrics.metadata.Parser.ElementsImplicit.elementsToSeq
  import org.allenai.scholar.metrics.metadata.Parser.StringImplicits
  import org.allenai.scholar.metrics.metadata.Parser.JsoupElementsImplicits

  /** Assumption that the first two last names, published year, and first word in the title uniquely
    * id a paper. Failure may occur, but very rarely.
    * @param cm The CoreMetadata object.
    * @return The hash/key of cm.
    */
  def bibKey(cm: CoreMetadata): String =
    cm.authorNames.take(2).map(_.lastNameFromFull).mkString("_") +
      cm.publishedYear +
      cm.title.takeWhile(_ != ' ')

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

    def extractSpecialBib(bib: Element): CoreMetadata

    protected def extractNames(e: Element, authorPath: String, initial: Boolean = false) =
      e.select(authorPath).map(a => {
        val last = a.extractName(lastRelativePath)
        val first = a.extractName(firstRelativePath)
        val middle = a.extractName(middleRelativePath)
        last.buildFullName(first, middle)
      }).toList

    private def extractBibs(doc: Document) = doc.select(bibMainPath).map(bib =>
      bib.extractBibTitle(bibTitlePath) match {
        case title if title.nonEmpty =>
          CoreMetadata(
            title = title,
            authorNames = extractNames(bib, bibAuthorPath),
            venue = extractVenue(bib),
            publishedYear = extractBibYear(bib)
          )
        case _ => extractSpecialBib(bib)
      }).toList

    def parseCoreMetadata(file: File): Option[CoreMetadata] = try {
      val xmlString = Source.fromFile(file, "UTF-8").getLines().mkString("\n")
      Some(parseCoreMetadataString(xmlString))
    } catch {
      case e: Exception =>
        println(s"Could not parse xml file ${file.getName}")
        e.printStackTrace()
        None
    }
    /** Function that parses an XML file to produce core metadata.
      * Names are lower-cased and trimmed of non-letter at the end.
      * @param xmlString The XML data as a string
      * @return The paper's core metadata.
      */
    def parseCoreMetadataString(xmlString: String): CoreMetadata = {
      val doc = Jsoup.parse(xmlString)
      CoreMetadata(
        title = doc.extractTitle(titlePath),
        authorNames = extractNames(doc, authorPath),
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

    def extractSpecialBib(bib: Element): CoreMetadata =
      CoreMetadata(
        title = extractVenue(bib), // venue becomes title for PhD theses
        authorNames = extractNames(bib, "monogr>author"),
        venue = "",
        publishedYear = extractBibYear(bib)
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

    def extractSpecialBib(bib: Element): CoreMetadata =
      CoreMetadata(
        title = bib.extractBibTitle("booktitle"),
        authorNames = extractNames(bib, "authors>author"),
        venue = "",
        publishedYear = extractBibYear(bib)
      )
  }

}
