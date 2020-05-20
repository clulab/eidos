package org.clulab.wm.eidos.expansion

import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.State
import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.attachments.TriggeredAttachment
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.{Set => MutableSet}

class MostCompleteEventsKeeper {
  protected val emptyState = new State()

  def filterSubstringEntities(entities: Seq[TextBoundMention]): Seq[Mention] = {

    val entityGroups = entities.groupBy(event => (event.sentence, event.label))

    val filteredForTextSubsumption = for {
      (_, unsortedEntitiesInGroup) <- entityGroups
      // most args/longest/attachiest first
      entitiesInGroup = unsortedEntitiesInGroup.sortBy(ent => -/*(*/mentionAttachmentWeight(ent)) //) + ent.tokenInterval.length))
      entitiesKept = MutableSet[TextBoundMention]() // Cache intermediate events.
      filtered = entitiesInGroup.filter { entity =>
        // Check to see if it's subsumed by something already there
        if (!entitiesKept.exists(ent => subsumesInterval(Set(ent.tokenInterval), Set(entity.tokenInterval)))) {
          entitiesKept.add(entity) // Add this event because it isn't subsumed by what's already there.
          true // Keep the attachment.
        }
        else
          false
      }
    } yield filtered

    filteredForTextSubsumption.flatten.toSeq
  }

  def contained(s: String, a: Set[String]): Boolean = a.exists(elem => elem.contains(s))
  // True if A subsumes B
  def contained(s: Interval, a: Set[Interval]): Boolean = a.exists(elem => elem.contains(s))
  def subsumesString(a: Set[String], b: Set[String]): Boolean = b.forall(elem => contained(elem, a))
  def subsumesInterval(a: Set[Interval], b: Set[Interval]): Boolean = b.forall(elem => contained(elem, a))

  // The size of a mention is the sum of:
  //    i) how many attachments are present
  //    ii) sum of args in each of the attachments
  //    iii) if (EventMention) ==>then include size of arguments
  def mentionAttachmentWeight(mention: Mention): Int = {

    // number of Arguments, number of attachments, the set of all attachments
    val (numArgs, modSize, attachmentsSet) = mention match {
      case tb: TextBoundMention =>
        val tbModSize = tb.attachments.size * 10
        val tbAttachmentSet = tb.attachments
        (0, tbModSize, tbAttachmentSet)
      case rm: RelationMention =>
        val rmSize = rm.arguments.values.flatten.size * 100
        val rmModSize = rm.arguments.values.flatten.map(arg => arg.attachments.size).sum * 10
        val rmAttachmentSet = rm.arguments.values.flatten.flatMap(m => m.attachments).toSet
        (rmSize, rmModSize, rmAttachmentSet)
      case em: EventMention =>
        val emSize = em.arguments.values.flatten.size * 100
        val emModSize = em.arguments.values.flatten.map(arg => arg.attachments.size).sum * 10
        val emAttachmentSet = em.arguments.values.flatten.flatMap(m => m.attachments).toSet
        (emSize, emModSize, emAttachmentSet)
      case _ => (0, 0, mention.attachments)
    }

    val argumentSize = attachmentsSet.toSeq.map(_.asInstanceOf[EidosAttachment].argumentSize).sum
    val triggerSize = mention.attachments.toSeq.collect{ case a: TriggeredAttachment => a.trigger.length}.sum
    val attachArgumentsSz = argumentSize + triggerSize

    val res = attachArgumentsSz + modSize + numArgs + mention.tokenInterval.length

    res
  }

  // We need to remove underspecified EventMentions of near-duplicate groupings
  // (ex. same phospho, but one is missing a site)
  def argTokenInterval(m: EventMention): Interval =  {

    if (m.arguments.keys.nonEmpty) {
      val min =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.start).toList.min
      val max =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.end).toList.max
      Interval(start = min, end = max)
    }
    else {
      MostCompleteEventsKeeper.logger.warn("Event with no arguments.")
      Interval(start = m.trigger.tokenInterval.start, end = m.trigger.tokenInterval.end)
    }
  }

  def filterSubstringArgumentEvents(events: Seq[EventMention]): Seq[Mention] = {

    def argumentTexts(em: EventMention): Set[String] = em.arguments.values.toSet.flatten.map(m => m.text)

    // Return true if the event subsumes all the args
    def eventArgsSubsume(argsToCheck: Set[String], event: EventMention): Boolean = {
      val eventArgStrings = argumentTexts(event)
      subsumesString(eventArgStrings, argsToCheck)
    }


    val triggerGroups = events.groupBy(event => (event.sentence, event.label, event.trigger.tokenInterval))

    val filteredForArgumentSubsumption = for {
      (_, unsortedEventsInGroup) <- triggerGroups
      // most args/longest/attachiest first
      eventsInGroup = unsortedEventsInGroup.sortBy(event => -(mentionAttachmentWeight(event) + argTokenInterval(event).length))
      eventsKept = MutableSet[EventMention]() // Cache intermediate events.
      filtered = eventsInGroup.filter { event =>
        // Check to see if it's subsumed by something already there
        val argTexts = argumentTexts(event)

        if (!eventsKept.exists(ev => eventArgsSubsume(argTexts, ev))) {
          eventsKept.add(event) // Add this event because it isn't subsumed by what's already there.
          true // Keep the attachment.
        }
        else
          false
      }
    } yield filtered

    filteredForArgumentSubsumption.toSeq.flatten
  }

  // This was essentially .head before, but that is dependent on order.
  def tieBreaker(mentions: Seq[Mention]): Mention = {
    //val oldBest = mentions.head
    val newBest = mentions.minBy(_.foundBy)

    //    if (!oldBest.eq(newBest))
    //      logger.debug("Changed answer")
    newBest
  }

  def customAttachmentFilter(mentions: Seq[Mention]): Seq[Mention] = {

    // --- To distinguish between :
    // 1. Attachments: Quantification(high,Some(Vector(record))), Increase(high,None)
    // 2. Attachments: Quantification(high,None), Increase(high,None)
    // --- and select 2.

    val mention_attachmentSz: Seq[(Mention, Int)] = mentions.map(m => (m, mentionAttachmentWeight(m)))

    val maxModAttachSz = mention_attachmentSz.map(_._2).max
    val filteredMentions = mention_attachmentSz.filter(m => m._2 == maxModAttachSz).map(_._1)
    filteredMentions
  }

  // Remove incomplete Mentions
  def keepMostCompleteEvents(ms: Seq[Mention]): Seq[Mention] = {
    val (baseEvents, nonEvents) = ms.partition(_.isInstanceOf[EventMention])
    // Filter out duplicate (or subsumed) events.  Strict containment used -- i.e. simply overlapping args is not
    // enough to be filtered out here.
    val events = filterSubstringArgumentEvents(baseEvents.map(_.asInstanceOf[EventMention]))

    // Entities
    val (baseTextBounds, relationMentions) = nonEvents.partition(_.isInstanceOf[TextBoundMention])
    val textBounds = filterSubstringEntities(baseTextBounds.map(_.asInstanceOf[TextBoundMention]))

    // remove incomplete entities (i.e. under specified when more fully specified exists)
    val tbMentionGroupings =
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, m.label, m.sentence))

    val completeTBMentions =
      for ((_, tbms) <- tbMentionGroupings) yield {

        val filteredTBMs = customAttachmentFilter(tbms)
        // In case there are several, use the one one smallest according to the rule.
        tieBreaker(filteredTBMs)
      }

    // Events
    val eventMentionGroupings =
      events.map(_.asInstanceOf[EventMention]).groupBy(m => (m.label, argTokenInterval(m), m.sentence))

    // remove incomplete event mentions
    val completeEventMentions =
      for ((_, ems) <- eventMentionGroupings) yield {
        // max number of arguments
        //val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        // max number of argument modifications
        // todo not all attachments are equal
        val filteredEMs = customAttachmentFilter(ems)
        tieBreaker(filteredEMs)
      }

    completeTBMentions.toSeq ++ relationMentions ++ completeEventMentions.toSeq
  }
}

object MostCompleteEventsKeeper {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
