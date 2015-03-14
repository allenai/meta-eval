package org.allenai.scholar.metrics.metadata

import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }

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
  publishedYear: Year = defaultPublishedYear,
  venue: String = "",
  bibs: List[CoreMetadata] = List()
)

object CoreMetadata {

  import org.allenai.scholar.metrics.metadata.ElementsImplicit._

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

    def extractNames(e: Element, authorPath: String, initial: Boolean = false) =
      e.select(authorPath).map(a => {
        val last = a.extractName(lastRelativePath)
        val first = a.extractName(firstRelativePath)
        val middle = a.extractName(middleRelativePath)
        last.buildFullName(first, middle)
      }).sorted.toList

    def extractBibs(doc: Document) = doc.select(bibMainPath).map(bib =>
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

    /** Function that parses an XML file to produce core metadata.
      * Names are lower-cased and trimmed of non-letter at the end.
      * @param file The XML file
      * @return The paper's core metadata.
      */
    def parseCoreMetadata(file: File): Option[CoreMetadata] = try {
      val doc = Jsoup.parse(file, "UTF-8", "")
      Some(CoreMetadata(
        title = doc.extractTitle(titlePath),
        authorNames = extractNames(doc, authorPath),
        bibs = extractBibs(doc)
      ))
    } catch {
      case e: Exception =>
        println(s"Could not parse xml file ${file.getName}")
        e.printStackTrace()
        None
    }
  }

  object grobidParser extends Parser(
    titlePath = "teiHeader>fileDesc>titleStmt>title",
    authorPath = "teiHeader>fileDesc>sourceDesc>biblStruct>analytic>author",
    lastRelativePath = "persName>surname",
    firstRelativePath = "persName>forename[type=first]",
    middleRelativePath = "persName>forename[type=middle]",
    bibMainPath = "listBibl>biblStruct",
    bibAuthorPath = "analytic>author",
    bibTitlePath = "analytic>title[type=main]"
  ) {

    override def extractBibYear(bib: Element) =
      bib.extractYear("monogr>imprint>date[type=published]", _.attr("when"))

    def extractVenue(bib: Element) = bib.extractBibTitle("monogr>title")

    override def extractSpecialBib(bib: Element) =
      CoreMetadata(
        title = extractVenue(bib), // venue becomes title for PhD theses
        authorNames = extractNames(bib, "monogr>author"),
        venue = "",
        publishedYear = extractBibYear(bib)
      )
  }

  object metataggerParser extends Parser(
    titlePath = "document>content>headers>title",
    authorPath = "document>content>headers>authors>author",
    lastRelativePath = "author-last",
    firstRelativePath = "author-first",
    middleRelativePath = "author-middle",
    bibMainPath = "biblio>reference",
    bibAuthorPath = "authors>author",
    bibTitlePath = "title"
  ) {

    override def extractBibYear(bib: Element) = bib.extractYear("date", _.text)

    override def extractVenue(bib: Element) =
      List("conference", "journal", "booktitle")
        .find(vt => !bib.select(vt).isEmpty) match {
          case Some(v) => bib.extractBibTitle(v)
          case None => ""
        }

    override def extractSpecialBib(bib: Element) =
      CoreMetadata(
        title = bib.extractBibTitle("booktitle"),
        authorNames = extractNames(bib, "authors>author"),
        venue = "",
        publishedYear = extractBibYear(bib)
      )
  }

}
