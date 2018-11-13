package org.clulab.wm.eidos.actions

import org.clulab.odin._
import org.clulab.wm.eidos.attachments.EidosAttachment

/** All *event* actions in Eidos must implement this trait */
trait EidosBaseActions extends Actions {

  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention]

  def keepMostCompleteEvents(mentions: Seq[Mention], state: State): Seq[Mention]





  // This was essentially .head before, but that is dependent on order.
  def tieBreaker(mentions: Seq[Mention]): Mention = {
    val oldBest = mentions.head
    val newBest = mentions.minBy(_.foundBy)

    //    if (!oldBest.eq(newBest))
    //      println("Changed answer")
    newBest
  }

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  // todo BECKY: if trigger is also a quant (i.e. has a quant attachment), add as quant in inc/dec
  def applyAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for {
      m <- ms
      //if m matches "EntityModifier"
      attachment = getAttachment(m)

      copyWithMod = m match {
        case tb: TextBoundMention => tb.copy(attachments = tb.attachments ++ Set(attachment), foundBy = s"${tb.foundBy}++mod")
        // Here, we want to keep the theme that is being modified, not the modification event itself
        case rm: RelationMention =>
          val theme = tieBreaker(rm.arguments("theme")).asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${rm.foundBy}")
        case em: EventMention =>
          val theme = tieBreaker(em.arguments("theme")).asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${em.foundBy}")
      }
    } yield copyWithMod
  }

  def getAttachment(mention: Mention): EidosAttachment = EidosAttachment.newEidosAttachment(mention)
}
