package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention

trait PostProcessing {
  def process(annotatedDocument: AnnotatedDocument): AnnotatedDocument
}

// The odinMentions are probably "surface" mentions, not all the reachable mentions.
class AnnotatedDocument(val document: Document, val odinMentions: Seq[Mention], val eidosMentions: Seq[EidosMention]) {
  // Note that asEidosMentions returns surface mentions only, not all reachable mentions.
  def this(document: Document, odinMentions: Seq[Mention]) = this(document, odinMentions, EidosMention.asEidosMentions(odinMentions))

  lazy val allOdinMentions: Seq[Mention] = EidosMention.findReachableOdinMentions(odinMentions)
  lazy val allEidosMentions: Seq[EidosMention] = EidosMention.findReachableEidosMentions(eidosMentions)
}

object AnnotatedDocument {
  type PreProcessing = Seq[Mention] => Seq[Mention]
  type Corpus = Seq[AnnotatedDocument]

  def apply(document: Document, odinMentions: Seq[Mention]) = new AnnotatedDocument(document, odinMentions)
}
