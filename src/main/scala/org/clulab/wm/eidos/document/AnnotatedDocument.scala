package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.OdinMention

class AnnotatedDocument(val document: Document, val eidosMentions: Seq[EidosMention], val allEidosMentions: Seq[EidosMention]) {
  lazy val odinMentions: Seq[Mention] = eidosMentions.map(_.odinMention)
  lazy val allOdinMentions: Seq[Mention] = allEidosMentions.map(_.odinMention)
}

object AnnotatedDocument {
  type Corpus = Seq[AnnotatedDocument]

  // The someEidosMentions are probably "surface" mentions, not all the reachable mentions.
  // Furthermore, there could be duplicates.  See asEidosMentions for details.
  def apply(document: Document, someOdinMentions: Seq[Mention]): AnnotatedDocument = {
    val (someEidosMentions, allEidosMentions) = EidosMention.asEidosMentions(someOdinMentions)
    val annotatedDocument = new AnnotatedDocument(document, someEidosMentions, allEidosMentions)

    annotatedDocument
  }
}
