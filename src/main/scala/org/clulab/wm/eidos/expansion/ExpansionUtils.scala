package org.clulab.wm.eidos.expansion

import org.clulab.odin._
import org.clulab.wm.eidos.attachments.AttachmentHandler
import org.clulab.wm.eidos.attachments.DCTime
import org.clulab.wm.eidos.attachments.Property
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.mentions.MentionUtils
import org.clulab.wm.eidos.utils.FoundBy

object ExpansionUtils {

  /*
      Attachments helper methods
   */

  // During expansion, sometimes there are attachments that got sucked up, here we add them to the expanded argument mention
  def addSubsumedAttachments(expanded: Mention, state: State): Mention = {
    // find mentions of the same label and sentence overlap
    val overlapping = state.mentionsFor(expanded.sentence, expanded.tokenInterval)
    // new foundBy for paper-trail, removes duplicate portions
    val completeFoundBy = if (overlapping.nonEmpty) FoundBy.concat(overlapping) else expanded.foundBy
    // get all the attachments for the overlapping mentions
    val allAttachments = overlapping.flatMap(m => m.attachments).distinct
    // make attachments out of the properties todo: should we have already done this?
    val propertyAttachments = getOverlappingPropertyAttachments(expanded, state)
    // filter out substring attachments
    val filtered = AttachmentHandler.filterAttachments(allAttachments ++ expanded.attachments.toSeq ++ propertyAttachments)
    // Add in all attachments
    val withAttachments = MentionUtils.withOnlyAttachments(expanded, filtered)
    // Modify the foundby for paper trail
    MentionUtils.withFoundBy(withAttachments, completeFoundBy)
  }

  // Add the document creation time (dct) attachment if there is no temporal attachment
  // i.e., a backoff
  def attachDCT(m: Mention, state: State): Mention = {
    val dctOpt = DctDocumentAttachment.getDct(m.document)
    if (dctOpt.isDefined && m.attachments.filter(_.isInstanceOf[Time]).isEmpty) {
      m.withAttachment(DCTime(dctOpt.get))
    }
    else
      m
  }

  def getOverlappingPropertyAttachments(m: Mention, state: State): Seq[Attachment] = {
    val interval = m.tokenInterval
    // TODO: Currently this is only Property attachments, but we can do more too
    val overlappingProps = state.mentionsFor(m.sentence, interval, label = "Property")
    overlappingProps.map(pm => Property(pm.text, None))
  }

}
