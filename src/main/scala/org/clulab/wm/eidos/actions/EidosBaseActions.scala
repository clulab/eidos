package org.clulab.wm.eidos.actions

import org.clulab.odin._
import org.clulab.wm.eidos.attachments.EidosAttachment

/** All *event* actions in Eidos must implement this trait */
trait EidosBaseActions extends Actions {

  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention]

  // FIXME: Can the detection of attachments be performed prior to event extraction?
  def keepMostCompleteEvents(mentions: Seq[Mention], state: State): Seq[Mention] = AttachmentHandler.keepMostCompleteEvents(mentions, state)

  // FIXME: Can the detection of attachments be performed prior to event extraction?
  def applyAttachment(mentions: Seq[Mention], state: State): Seq[Mention] = AttachmentHandler.applyAttachment(mentions, state)


}
