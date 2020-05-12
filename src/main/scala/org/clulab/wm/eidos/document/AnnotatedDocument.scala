package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention

// The someEidosMentions are probably "surface" mentions, not all the reachable mentions.
class AnnotatedDocument(val document: Document, val eidosMentions: Seq[EidosMention], val allEidosMentions: Seq[EidosMention]) {
  lazy val odinMentions: Seq[Mention] = eidosMentions.map { eidosMention => eidosMention.odinMention }
  lazy val allOdinMentions: Seq[Mention] = EidosMention.findReachableOdinMentions(odinMentions)
}

object AnnotatedDocument {
  type Corpus = Seq[AnnotatedDocument]

  def fromOdinMentions(document: Document, someOdinMentions: Seq[Mention]): AnnotatedDocument = {
    val (someEidosMentions, allEidosMentions) = EidosMention.asEidosMentions(someOdinMentions)
    val annotatedDocument = AnnotatedDocument(document, someEidosMentions, allEidosMentions)

    annotatedDocument
  }

  def apply(document: Document, someEidosMentions: Seq[EidosMention]): AnnotatedDocument =
      apply(document, someEidosMentions, EidosMention.findReachableEidosMentions(someEidosMentions))

  def apply(document: Document, someEidosMentions: Seq[EidosMention], allEidosMentions: Seq[EidosMention]): AnnotatedDocument =
      new AnnotatedDocument(document, someEidosMentions, allEidosMentions)
}
