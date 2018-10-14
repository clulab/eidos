package org.clulab.wm.eidos.utils

import org.clulab.odin._

object MentionUtils {

  def newCrossSentenceMention(m: CrossSentenceMention, attachments: Set[Attachment]) =
      new CrossSentenceMention(m.labels, m.anchor, m.neighbor, m.arguments, m.document, m.keep, m.foundBy, attachments)

  protected def withOnlyAttachments(mention: Mention, setOfAttachments: Set[Attachment]): Mention =
      mention match {
        case m: TextBoundMention => m.copy(attachments = setOfAttachments)
        case m: RelationMention => m.copy(attachments = setOfAttachments)
        case m: EventMention => m.copy(attachments = setOfAttachments)
        case m: CrossSentenceMention => newCrossSentenceMention(m, setOfAttachments)
      }

  def withOnlyAttachments(mention: Mention, seqOfAttachments: Seq[Attachment]): Mention =
      withOnlyAttachments(mention, seqOfAttachments.toSet)

  def withMoreAttachments(mention: Mention, attachments: Seq[Attachment]): Mention =
      withOnlyAttachments(mention, mention.attachments ++ attachments)
}
