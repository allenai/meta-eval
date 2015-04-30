import org.allenai.scholar.metrics.metadata.GrobidParser

import scala.io.Source

/** Created by rodneykinney on 4/29/15.
  */
object Scratch {
  def main(args: Array[String]): Unit = {
    val xml = Source.fromFile("/Users/rodneykinney/data/meta-eval/data/acl/extracted/scratch/P05-1045.tei.xml").mkString
    val doc = GrobidParser.extractStructuredDocument(xml)
    println(doc)

  }

}
