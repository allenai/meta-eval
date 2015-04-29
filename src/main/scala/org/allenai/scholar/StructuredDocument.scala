package org.allenai.scholar

import org.allenai.scholar.metrics.metadata.PaperMetadata

/** Created by rodneykinney on 4/29/15.
  */
case class StructuredDocument(
    metadata: PaperMetadata,
    paperAbstract: Option[String],
    body: IndexedSeq[Section],
    bibliography: Bibliography
) {

}

case class Section(id: Option[String], header: Option[String], text: String)

case class Bibliography(entries: IndexedSeq[(PaperMetadata, Iterable[Mention])])

case class Mention(containingText: String, begin: Int, end: Int)
