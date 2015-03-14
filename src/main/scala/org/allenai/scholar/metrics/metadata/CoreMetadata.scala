package org.allenai.scholar.metrics.metadata

import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import org.jsoup.select.Elements

import scala.collection.mutable

import java.io.File
import java.time.Year

object CoreExtractor extends Enumeration {
  type CoreExtractor = Value
  val GROBID, METATAGGER = Value
}

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

  object ElementsImplicit {

    import scala.collection.JavaConverters._

    /** Convenient implicit so don't need to write .asScala any time calling jsoup's select.
      */
    implicit def elementsToSeq(elms: Elements): mutable.Buffer[Element] = elms.asScala
  }

  import org.allenai.scholar.metrics.metadata.CoreMetadata.ElementsImplicit._
  import org.allenai.scholar.metrics.metadata.CoreExtractor._

  val titlePath = Map(
    GROBID -> "teiHeader>fileDesc>titleStmt>title",
    METATAGGER -> "document>content>headers>title"
  )

  val authorPath = Map(
    GROBID -> "teiHeader>fileDesc>sourceDesc>biblStruct>analytic>author",
    METATAGGER -> "document>content>headers>authors>author"
  )

  val lastRelativePath = Map(
    GROBID -> "persName>surname",
    METATAGGER -> "author-last"
  )

  val firstRelativePath = Map(
    GROBID -> "persName>forename[type=first]",
    METATAGGER -> "author-first"
  )

  val middleRelativePath = Map(
    GROBID -> "persName>forename[type=middle]",
    METATAGGER -> "author-middle"
  )

  val bibMainPath = Map(
    GROBID -> "listBibl>biblStruct",
    METATAGGER -> "biblio>reference"
  )

  val bibAuthorPath = Map(
    GROBID -> "analytic>author",
    METATAGGER -> "authors>author"
  )

  val grobidMonogrBibAuthorPath = "monogr>author"

  val bibTitlePath = Map(
    GROBID -> "analytic>title[type=main]",
    METATAGGER -> "title"
  )

  val bibBookTitlePath = Map(
    GROBID -> "analytic>title[type=main]",
    METATAGGER -> "booktitle"
  )

  def extractYear(s: String) = "\\d{4}".r.findFirstIn(s) match {
    case Some(y) => Year.parse(y)
    case None => defaultPublishedYear
  }

  def extractMetataggerYear(bib: Element) = bib.select("date").headOption match {
    case Some(d) => extractYear(d.text)
    case None => defaultPublishedYear
  }

  def extractGrobidYear(bib: Element) =
    bib.select("monogr>imprint>date[type=published]").headOption match {
      case Some(y) => "\\d{4}".r.findFirstIn(y.attr("when")) match {
        case Some(yearStr) => Year.parse(yearStr)
        case None => defaultPublishedYear
      }
      case None => defaultPublishedYear
    }

  def extractMetataggerVenue(bib: Element) =
    List("conference", "journal", "booktitle")
      .find(vt => !bib.select(vt).isEmpty) match {
        case Some(v) => bib.select(v).head.toLowerCaseText
        case None => ""
      }

  def extractTitle(bib: Element, path: String) =
    bib.select(path).headOption match {
      case Some(v) => v.toLowerCaseText
      case None => ""
    }

  /** Typically try to extract a paper's revenue from the bib section. Sometimes ends up
    * extracting the title instead (e.g. in the case of PhD thesis).
    */
  def extractGrobidMonogrTitle(bib: Element) = extractTitle(bib, "monogr>title")

  def extractName(a: Element, namePath: String) = a.select(namePath).headOption match {
    case Some(n) => n.toLowerCaseTextTrimNonLetters
    case None => ""
  }

  def extractNames(e: Element, authorPath: String, tagger: CoreExtractor, initial: Boolean = false) =
    e.select(authorPath).map(a => {
      val last = extractName(a, lastRelativePath(tagger))
      val first = extractName(a, firstRelativePath(tagger))
      val middle = extractName(a, middleRelativePath(tagger))
      last.buildFullName(first, middle)
    }).sorted.toList

  def extractBibs(doc: Document, tagger: CoreExtractor) = doc.select(bibMainPath(tagger)).map(bib =>
    extractTitle(bib, bibTitlePath(tagger)) match {
      case title if title.nonEmpty =>
        val (venue, year) = tagger match {
          case GROBID => (extractGrobidMonogrTitle(bib), extractGrobidYear(bib))
          case METATAGGER => (extractMetataggerVenue(bib), extractMetataggerYear(bib))
        }
        CoreMetadata(
          title = title,
          authorNames = extractNames(bib, bibAuthorPath(tagger), tagger),
          venue = venue,
          publishedYear = year
        )
      case _ =>
        CoreMetadata(
          // Rather messy handling of Grobid bibs due to inconsistent schema
          title = extractGrobidMonogrTitle(bib),
          authorNames = extractNames(bib, grobidMonogrBibAuthorPath, tagger),
          venue = "",
          publishedYear = extractGrobidYear(bib)
        )
    }).toList

  /** Function that parses an XML file to produce core metadata.
    * Names are lower-cased and trimmed of non-letter at the end.
    * @param file The XML file
    * @return The paper's core metadata.
    */
  def parseCoreMetadata(tagger: CoreExtractor)(file: File): Option[CoreMetadata] = try {
    val doc = Jsoup.parse(file, "UTF-8", "")
    Some(CoreMetadata(
      title = extractTitle(doc, titlePath(tagger)),
      authorNames = extractNames(doc, authorPath(tagger), tagger),
      bibs = extractBibs(doc, tagger)
    ))
  } catch {
    case e: Exception =>
      println(s"Could not parse xml file ${file.getName}")
      e.printStackTrace()
      None
  }
}
