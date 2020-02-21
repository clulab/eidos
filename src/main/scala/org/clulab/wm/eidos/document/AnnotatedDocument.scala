package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention

// The eidosMentions are probably "surface" mentions, not all the reachable mentions.
class AnnotatedDocument(val document: Document, val eidosMentions: Seq[EidosMention]) {
  lazy val odinMentions: Seq[Mention] = eidosMentions.map { eidosMention => eidosMention.odinMention }
  lazy val allOdinMentions: Seq[Mention] = EidosMention.findReachableOdinMentions(odinMentions)
  lazy val allEidosMentions: Seq[EidosMention] = EidosMention.findReachableEidosMentions(eidosMentions)
}

object AnnotatedDocument {
  type Corpus = Seq[AnnotatedDocument]

  def apply(document: Document, eidosMentions: Seq[EidosMention]) = new AnnotatedDocument(document, eidosMentions)
}
