package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.{EidosMention, OdinMention}

// The someEidosMentions are probably "surface" mentions, not all the reachable mentions.
class AnnotatedDocument(val document: Document, val eidosMentions: Seq[EidosMention], val allEidosMentions: Seq[EidosMention]) {
  // Do they need to be deduplicated then because could get pointer to two that are very same one?
  lazy val odinMentions: Seq[Mention] = eidosMentions.map { eidosMention => eidosMention.odinMention }
  // The odinMentions of each eidosMention should have been deduplicated by now, so can find by identity?
  // Anything that was equal before should be mapped to a single one that goes for the group.
  lazy val allOdinMentions: Seq[Mention] = OdinMention.findAllByEquality(odinMentions)
}

object AnnotatedDocument {
  type Corpus = Seq[AnnotatedDocument]

  def apply(document: Document, someOdinMentions: Seq[Mention]): AnnotatedDocument = {
    val (someEidosMentions, allEidosMentions) = EidosMention.asEidosMentions(someOdinMentions)
    val annotatedDocument = new AnnotatedDocument(document, someEidosMentions, allEidosMentions)

    annotatedDocument
  }
}
