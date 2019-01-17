package org.clulab.wm.eidos.document

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention

case class AnnotatedDocument(document: Document, odinMentions: Seq[Mention], eidosMentions: Seq[EidosMention])

object AnnotatedDocument {
  type Corpus = Seq[AnnotatedDocument]
}
