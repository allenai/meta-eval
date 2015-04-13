package org.allenai.scholar

import org.allenai.scholar.metrics.metadata.PaperMetadata

case class MetadataAndBibliography(
  metadata: PaperMetadata,
  bibs: Seq[PaperMetadata] = List()
)

object MetadataAndBibliography {
  def edgesToBibKeyMap(
    citationEdges: Iterable[(String, String)],
    coreMetadata: Map[String, PaperMetadata]
  ): Map[String, Map[String, PaperMetadata]] = {
    val edges = for {
      (citing, cited) <- citationEdges
      citedMeta <- coreMetadata.get(cited)
    } yield {
      (citing, (cited, citedMeta))
    }
    edges
      .groupBy(_._1) // group by citing paper id
      .mapValues(_.map(_._2).toMap) // each value is a map from citee's bibKey to its CoreMetadata
  }
}