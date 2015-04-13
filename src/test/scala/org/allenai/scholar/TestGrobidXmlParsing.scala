package org.allenai.scholar

import java.time.Year

import org.allenai.common.Resource
import org.allenai.common.testkit.UnitSpec
import org.allenai.scholar.metrics.metadata.GrobidParser

import scala.io.Source

class TestGrobidXmlParsing extends UnitSpec {
  val MetadataAndBibliography(metadata, bibs) =
    GrobidParser.parseCoreMetadataString(IoHelpers.loadTeiFile("P07-2045"))

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
    //  missing Alexandra Constantin and
    // Ondřej Bojar
    metadata.authors.toSet should equal(expectedAuthors)
  }
}

object IoHelpers {
  def loadTeiFile(id: String): String = {
    Resource.using(Source.fromInputStream(
      getClass.getResourceAsStream(s"/$id.tei.xml")
    ))(_.mkString)
  }
}