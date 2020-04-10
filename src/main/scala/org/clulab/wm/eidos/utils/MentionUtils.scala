package org.clulab.wm.eidos.utils

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.{EidosAttachment, Negation, TriggeredAttachment}
import org.clulab.wm.eidos.mentions.EidosMention

import scala.collection.mutable.{Set => MutableSet}

case class TriggerInfo(text: String, start: Int, end: Int)

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

  def withFoundBy(mention: Mention, foundBy: String): Mention = {
    mention match {
      case tb: TextBoundMention => tb.copy(foundBy=foundBy)
      case rm: RelationMention => rm.copy(foundBy=foundBy)
      case em: EventMention => em.copy(foundBy=foundBy)
    }
  }

  def hasNegation(m: EidosMention): Boolean = hasNegation(m.odinMention)
  def hasNegation(m: Mention): Boolean = m.attachments.exists(_.isInstanceOf[Negation])

  def triggerOpt(m: EidosMention): Option[String] = triggerOpt(m.odinMention)
  def triggerOpt(m: Mention): Option[String] = m match {
    case em: EventMention => Some(em.trigger.text)
    case _ => None
  }

  def triggerInfo(em: EidosMention): TriggerInfo = triggerInfo(em.odinMention)
  def triggerInfo(m: Mention): TriggerInfo = m match {
    // if mention is an EventMention, it's easy
    case em: EventMention => TriggerInfo(em.trigger.text, em.trigger.startOffset, em.trigger.endOffset)
    // Otherwise, check to see if the mention has a syntactic head
    case _ => {
      val synHeadOpt = m.synHead
      if (synHeadOpt.isDefined) {
        // get the info from the syntactic head
        val head = synHeadOpt.get
        val s = m.sentenceObj
        TriggerInfo(s.words(head), s.startOffsets(head), s.endOffsets(head))
      } else {
        // Otherwise backoff to the whole mention
        TriggerInfo(m.text, m.startOffset, m.endOffset)
      }
    }
  }


}
