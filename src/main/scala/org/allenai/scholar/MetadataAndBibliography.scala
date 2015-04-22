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
  ): Map[String, Set[PaperMetadata]] = {
    val edges = for {
      (citing, cited) <- citationEdges
      citedMeta <- coreMetadata.get(cited)
    } yield (citing, citedMeta)

    edges
      .groupBy(_._1)
      .mapValues(metas => metas.map(m => m._2).toSet)
  }
}