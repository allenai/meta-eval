package org.allenai.scholar

import java.time.Year

import org.allenai.common.Resource
import org.allenai.common.testkit.UnitSpec
import org.allenai.scholar.metrics.metadata.GrobidParser
import org.jsoup.Jsoup

import scala.io.Source

class TestGrobidXmlParsing extends UnitSpec {
  val MetadataAndBibliography(metadata, bibs) =
    GrobidParser.extractMetadataAndBib(IoHelpers.loadTeiFile("P07-2045"))

  "Grobid output" should "parse metadata correctly" in {
    val expectedTitle = Title("Moses: Open Source Toolkit for Statistical Machine Translation")
    metadata.title should equal(expectedTitle)
    metadata.venue should equal(Venue("Proceedings of the ACL 2007 Demo and Poster Sessions"))
    metadata.year should equal(Year.of(2007))
    val expectedAuthors = Set(
      Author("Alexandra", List(), "Birch"),
      Author("Wade", List(), "Shen"),
      Author("Evan", List("Herbst"), "Cornell"), // should be "Evan Herbst" with Cornell aff.
      Author("Brooke", List(), "Cowan"),
      Author("Christine", List(), "Moran"),
      Author("Rwth", List(), "Aachen"), // this is an affiliation
      Author("Nicola", List(), "Bertoldi"),
      Author("Hieu", List(), "Hoang"),
      Author("Mit", List(), ""), // also affiliation
      Author("Chris", List(), "Dyer"),
      Author("Philipp", List(), "Koehn"),
      Author("Chris", List(), "Callison-Burch"),
      Author("Marcello", List(), "Federico"),
      Author("Itc-Irst", List(), ""), // another affiliation
      Author("Richard", List(), "Zens")
    )
    // missing Alexandra Constantin and Ondřej Bojar
    metadata.authors.toSet should equal(expectedAuthors)
  }

  "StructuredDoc" should "be extracted correctly" in {
    val doc = GrobidParser.extractStructuredDocument(IoHelpers.loadTeiFile("P05-1045"))

    doc.sections(0).header should equal(Some("Introduction"))
    doc.sections(0).text should not startWith ("Introduction")

    doc.sections.last.header should equal(Some("Conclusions"))

    val b15Mentions = doc.bibliography.entries(15)._2
    b15Mentions.size should equal(2)
    val List(m1, m2) = b15Mentions.toList
    doc.sections(m1.sectionNumber).text.substring(m1.begin, m1.end) should equal("Lafferty et al., 2001")
    doc.sections(m2.sectionNumber).text.substring(m2.begin, m2.end) should equal("Lafferty et al. (2001)")

    doc.footnotes.size should equal(4)
  }

  "Text offset" should "be computed correctly" in {
    import GrobidParser.Implicits._
    import scala.collection.JavaConverters._

    def compareOffset(xmlString: String, elementPath: String, ancestorPath: String, count: Int = 1): Unit = {
      val parsed = Jsoup.parse(xmlString, "", org.jsoup.parser.Parser.xmlParser())
      val e = parsed.select(elementPath).asScala.head
      val ancestor = parsed.select(ancestorPath).head
      val textOffsetViaXml = e.textOffset(ancestor)
      val textOffsetViaString = {
        val ancestorText = ancestor.text
        val elementText = e.text
        var index = ancestorText.indexOf(elementText)
        var n = count
        while (n > 1) {
          index = ancestorText.indexOf(elementText, index + 1)
          n -= 1
        }
        index
      }
      textOffsetViaXml should equal(textOffsetViaString)
    }

    var xml = <A>
                base text begin
                <B>
                  <C>
                    mention
                  </C>
                </B>
                base text end
              </A>
    compareOffset(xml.toString, "A>B>C", "A")

    xml = <A>
            base text begin
            <B>middle mention <C>modified mention</C> </B>
            base text end
          </A>
    compareOffset(xml.toString, "A>B>C", "A")

    xml = <A>
            base text begin
            <B>
              middle mention
              <C>
                mention
              </C>
            </B>
            base text end
          </A>
    compareOffset(xml.toString, "A>B>C", "A", 2)

    xml = <A>
            no surrounding whitespace
            <B><C>mention</C></B>
            base text end
          </A>
    compareOffset(xml.toString, "A>B>C", "A")

  }

}

object IoHelpers {
  def loadTeiFile(id: String): String = {
    Resource.using(Source.fromInputStream(
      getClass.getResourceAsStream(s"/$id.tei.xml")
    ))(_.mkString)
  }
}