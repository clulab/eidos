package org.clulab.wm.eidos.expansion

import org.clulab.odin._
import org.clulab.wm.eidos.attachments.{DCTime, Property, Time}
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.utils.{DisplayUtils, MentionUtils}

object ExpansionUtils {

  /*
      Attachments helper methods
   */

  // During expansion, sometimes there are attachments that got sucked up, here we add them to the expanded argument mention
  def addSubsumedAttachments(expanded: Mention, state: State): Mention = {
    def compositionalFoundBy(ms: Seq[Mention]): String = {
      ms.map(_.foundBy).flatMap(ruleName => ruleName.split("\\+\\+")).distinct.mkString("++")
    }

    // find mentions of the same label and sentence overlap
    val overlapping = state.mentionsFor(expanded.sentence, expanded.tokenInterval)
    // new foundBy for paper-trail, removes duplicate portions
    val completeFoundBy = compositionalFoundBy(overlapping)
    // get all the attachments for the overlapping mentions
    val allAttachments = overlapping.flatMap(m => m.attachments).distinct
    // Add in all attachments
    val withAttachments = MentionUtils.withMoreAttachments(expanded, allAttachments)
    // Modify the foundby for paper trail
    MentionUtils.withFoundBy(withAttachments, completeFoundBy)
  }

  // Add the document creation time (dct) attachment if there is no temporal attachment
  // i.e., a backoff
  def attachDCT(m: Mention, state: State): Mention = {
    val dct = m.document.asInstanceOf[EidosDocument].dct
    if (dct.isDefined && m.attachments.filter(_.isInstanceOf[Time]).isEmpty)
      m.withAttachment(DCTime(dct.get))
    else
      m
  }

  // FIXME -- the difference between this method and addSubsumedAttachments above is small, can they be unified?
  def addOverlappingAttachmentsTextBounds(m: Mention, state: State): Mention = {
    m match {
      case tb: TextBoundMention =>
        val attachments = getOverlappingAttachments(tb, state)
        if (attachments.nonEmpty) tb.copy(attachments = tb.attachments ++ attachments) else tb
      case _ => m
    }

  }

  def getOverlappingAttachments(m: Mention, state: State): Set[Attachment] = {
    val interval = m.tokenInterval
    // TODO: Currently this is only Property attachments, but we can do more too
    val overlappingProps = state.mentionsFor(m.sentence, interval, label = "Property")
    overlappingProps.map(pm => Property(pm.text, None)).toSet
  }

}
