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

  def withLabel(mention: Mention, label: String): Mention = {
    val newLabels = label +: mention.labels
    mention match {
      // Unfortunately, Mention itself does not have a copy(WithLabels) method.
      case m: TextBoundMention => m.copy(newLabels)
      case m:  RelationMention => m.copy(newLabels)
      case m:     EventMention => m.copy(newLabels)
      case _ => ??? // not done for cross-sentence or anything else
    }
  }
}
