package org.clulab.wm.eidos.attachments

import org.clulab.odin.{Attachment, Mention, State}
import org.clulab.wm.eidos.expansion.MostCompleteEventsKeeper
import org.clulab.wm.eidos.mentions.MentionUtils

import scala.collection.mutable.{Set => MutableSet}

object AttachmentHandler {
  def apply(): AttachmentHandler = new AttachmentHandler

  // Here we pass all the attachments, and the TriggeredAttachments get filtered with the below method
  def filterAttachments(attachment: Seq[Attachment]): Seq[Attachment] = {
    val (triggered, other) = attachment.partition(_.isInstanceOf[TriggeredAttachment])
    filterTriggeredAttachments(triggered.collect{case t: TriggeredAttachment => t}) ++ other
  }

  // Filter out substring attachments, then keep most complete.
  def filterTriggeredAttachments(attachments: Seq[TriggeredAttachment]): Seq[TriggeredAttachment] = {
    attachments
      // Perform first mapping based on class
      .groupBy(_.getClass)
      // Filter out substring attachments
      .flatMap { case (_, attachments) => filterSubstringTriggers(attachments) }
      // Next map based on both class and trigger.
      .groupBy(attachment => (attachment.getClass, attachment.trigger))
      // Now that substrings are filtered, keep only most complete of each class-trigger-combo.
      .map { case (_, attachments) => filterMostComplete(attachments.toSeq) }
      .toSeq
  }

  // Keep the most complete attachment here.
  def filterMostComplete(attachments: Seq[TriggeredAttachment]): TriggeredAttachment =
    attachments.maxBy(_.argumentSize)

  // Filter out substring attachments.
  def filterSubstringTriggers(attachments: Seq[TriggeredAttachment]): Seq[TriggeredAttachment] = {
    val triggersKept = MutableSet[String]() // Cache triggers of intermediate results.

    attachments
      .sorted
      .reverse
      .filter { attachment =>
        val trigger = attachment.trigger

        if (!triggersKept.exists(_.contains(trigger))) {
          triggersKept.add(trigger) // Add this trigger.
          true // Keep the attachment.
        }
        else
          false
      }
  }

  // Get trigger from an attachment
  protected def triggerOf(attachment: Attachment): String = {
    attachment match {
      case inc: Increase => inc.trigger
      case dec: Decrease => dec.trigger
      case quant: Quantification => quant.trigger
      case _ => throw new UnsupportedClassVersionError()
    }
  }
}

class AttachmentHandler {

  val mostCompleteEventsKeeper = new MostCompleteEventsKeeper()

  // Currently used as a GLOBAL ACTION in EidosSystem:
  // Merge many Mentions of a single entity that have diff attachments, so that you have only one entity with
  // all the attachments.  Also handles filtering of attachments of the same type whose triggers are substrings
  // of each other.
  def mergeAttachments(mentions: Seq[Mention]): Seq[Mention] = {

    // Get all the entity mentions for this span (i.e. all the "rainfall in Spain" mentions)
    val (entities, nonentities) = mentions.partition(mention => mention matches "Entity")
    val entitiesBySpan = entities.groupBy(entity => (entity.sentence, entity.tokenInterval, entity.label))
    val mergedEntities = for {
      (_, entities) <- entitiesBySpan
      // These are now for the same span, so only one should win as the main one.
      flattenedTriggeredAttachments = entities.flatMap(_.attachments.collect{ case a: TriggeredAttachment => a })
      flattenedContextAttachments = entities.flatMap(_.attachments.collect{ case a: ContextAttachment => a })
      filteredAttachments = AttachmentHandler.filterTriggeredAttachments(flattenedTriggeredAttachments)
    }
      yield {
        val bestEntities =
          if (filteredAttachments.nonEmpty) {
            val bestAttachment = filteredAttachments.sorted.reverse.head
            // Since head was used above and there could have been a tie, == should be used below
            // The tie can be broken afterwards.
            entities.filter(_.attachments.exists(_ == bestAttachment))
          }
          else
            entities

        MentionUtils.withOnlyAttachments(mostCompleteEventsKeeper.tieBreaker(bestEntities), filteredAttachments ++ flattenedContextAttachments)
      }

    val res = mostCompleteEventsKeeper.keepMostCompleteEvents(mergedEntities.toSeq ++ nonentities)
    res
  }


}
