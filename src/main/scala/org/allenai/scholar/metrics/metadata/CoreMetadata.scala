package org.allenai.scholar.metrics.metadata

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import java.io.File
import java.time.Year

object CoreExtractor extends Enumeration {
  type CoreExtractor = Value
  val GROBID, METATAGGER = Value
}

/** Research papers' core metadata.
  * @param title The paper's title.
  * @param authorNames List of authors' (last name, full name).
  * @param publishedYear Year of publication.
  * @param venue The venue where the paper was published.
  * @param bibs A list of bibs/references in the paper.
  */
case class CoreMetadata(
  title: String,
  authorNames: List[(String, String)],
  publishedYear: Year = defaultPublishedYear,
  venue: String = "",
  bibs: List[CoreMetadata] = List()
)

object CoreMetadata {

  object ElementsImplicit {

    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    /** Convenient implicit so don't need to write .asScala any time calling jsoup's select.
      * @param elms
      * @return
      */
    implicit def elementsToSeq(elms: Elements): Seq[Element] = elms.asScala
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

  val lastNameRelativePath = Map(
    GROBID -> "persName>surname",
    METATAGGER -> "author-last"
  )

  val firstNameRelativePath = Map(
    GROBID -> "persName>forename[type=first]",
    METATAGGER -> "author-first"
  )

  val middleNameRelativePath = Map(
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

  /** Typically try to extract a paper's revenue from the bib section. Sometimes ends up
    * extracting the title instead (e.g. in the case of PhD thesis).
    * @param bib
    * @return
    */
  def extractGrobidVenue(bib: Element) =
    bib.select("monogr>title").headOption match {
      case Some(v) => v.toLowerCaseText
      case None => ""
    }

  /** Function that parses an XML file to produce core metadata.
    * Names are lower-cased and trimmed of non-letter at the end.
    * @param file The XML file
    * @return The paper's core metadata.
    */
  def parseCoreMetadata(tagger: CoreExtractor)(file: File): Option[CoreMetadata] = {
    println(s"processing ${file.getName}")
    try {
      val doc = Jsoup.parse(file, "UTF-8", "")
      def extractName(a: Element, namePath: String) = a.select(namePath).headOption match {
        case Some(n) => n.toLowerCaseTextTrimNonLetters
        case None => ""
      }

      def extractNames(e: Element, authorPath: String, initial: Boolean = false) = e.select(authorPath).map(a => {
        val lastName = extractName(a, lastNameRelativePath(tagger))
        val firstName = extractName(a, firstNameRelativePath(tagger))
        val middleName = extractName(a, middleNameRelativePath(tagger))

        def format(name: String) = if (initial) name(0) else name

        var fullName = lastName
        if (firstName.nonEmpty) fullName = fullName + s", ${format(firstName)}"
        if (middleName.nonEmpty) fullName = fullName + s" ${format(middleName)}"
        (lastName, fullName)
      }).sorted

      def extractBibs() = doc.select(bibMainPath(tagger)).map(bib => {
        val names = extractNames(bib, bibAuthorPath(tagger))
        if (names.nonEmpty) {
          val (venue, year) = tagger match {
            case GROBID => (extractGrobidVenue(bib), extractGrobidYear(bib))
            case METATAGGER => (extractMetataggerVenue(bib), extractMetataggerYear(bib))
          }

          val title = bib.select(bibTitlePath(tagger)).headOption match {
            case Some(t) => t.toLowerCaseText
            case None => extractGrobidVenue(bib)
          }

          CoreMetadata(
            title = title,
            authorNames = names.toList,
            venue = venue,
            publishedYear = year
          )
        } else {
          CoreMetadata(
            // Rather messy handling of Grobid bibs due to inconsistent schema
            title = extractGrobidVenue(bib),
            authorNames = extractNames(bib, grobidMonogrBibAuthorPath).toList,
            venue = "",
            publishedYear = defaultPublishedYear
          )
        }
      })

      Some(CoreMetadata(
        title = doc.select(titlePath(tagger)).head.toLowerCaseText(),
        authorNames = extractNames(doc, authorPath(tagger)).toList,
        bibs = extractBibs.toList
      ))
    } catch {
      case e: Exception => {
        println(s"Could not load xml file ${file.getName}")
        e.printStackTrace()
        None
      }
    }
  }
}
