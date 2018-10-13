package org.clulab.wm.eidos.utils

import org.clulab.odin._

object MentionUtils {

  def newCrossSentenceMention(m: CrossSentenceMention, attachments: Set[Attachment]) =
      new CrossSentenceMention(m.labels, m.anchor, m.neighbor, m.arguments, m.document, m.keep, m.foundBy, attachments)

  def withoutWithAttachments(mention: Mention, seqOfAttachments: Seq[Attachment]): Mention = {
    val allAttachments = seqOfAttachments.toSet

    mention match {
      case m: TextBoundMention => m.copy(attachments = allAttachments)
      case m: RelationMention => m.copy(attachments = allAttachments)
      case m: EventMention => m.copy(attachments = allAttachments)
      case m: CrossSentenceMention => newCrossSentenceMention(m, allAttachments)
    }
  }

  def withAttachments(mention: Mention, seqOfAttachments: Seq[Attachment]): Mention = {
    val setOfAttachments = seqOfAttachments.toSet

    mention match {
      case m: TextBoundMention => m.copy(attachments = setOfAttachments)
      case m: RelationMention => m.copy(attachments = setOfAttachments)
      case m: EventMention => m.copy(attachments = setOfAttachments)
      case m: CrossSentenceMention => newCrossSentenceMention(m, setOfAttachments)
    }
  }
}
