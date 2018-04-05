package org.clulab.wm.eidos

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils}
import org.clulab.struct.Interval
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.Ordering
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Set => MutableSet}
import scala.io.BufferedSource


// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping


class EidosActions(val taxonomy: Taxonomy) extends Actions with LazyLogging {

  /**
    * @author Gus Hahn-Powell
    * Copies the label of the lowest overlapping entity in the taxonomy
    */
  def customAttachmentFilter(mentions: Seq[Mention]): Seq[Mention] = {

    // --- To distinguish between :
    // 1. Attachments: Quantification(high,Some(Vector(record))), Increase(high,None)
    // 2. Attachments: Quantification(high,None), Increase(high,None)
    // --- and select 2.

    val mention_attachmentSz: Seq[(Mention, Int)] = for (mention <- mentions) yield {

      // number of Arguments, number of attachments, the set of all attachments
      val (numArgs, modSize, attachmentsSet) = mention match {
        case tb: TextBoundMention => {
          val tbModSize = tb.attachments.size * 10
          val tbAttachmentSet = tb.attachments
          (0, tbModSize, tbAttachmentSet)
        }
        case rm: RelationMention => {
          val rmSize = rm.arguments.values.flatten.size * 100
          val rmModSize = rm.arguments.values.flatten.map(arg => arg.attachments.size).sum * 10
          val rmAttachmentSet = rm.arguments.values.flatten.flatMap(m => m.attachments).toSet
          (rmSize, rmModSize, rmAttachmentSet)
        }
        case em: EventMention => {
          val emSize = em.arguments.values.flatten.size * 100
          val emModSize = em.arguments.values.flatten.map(arg => arg.attachments.size).sum * 10
          val emAttachmentSet = em.arguments.values.flatten.flatMap(m => m.attachments).toSet
          (emSize, emModSize, emAttachmentSet)
        }
        case _ => (0, 0,  mention.attachments)
      }

      // disgusting!

      val argumentSize = attachmentsSet.toSeq.map(_.asInstanceOf[EidosAttachment].argumentSize).sum
      val triggerSize = mention.attachments.toSeq.map(triggerOf(_).length).sum
      val attachArgumentsSz = argumentSize + triggerSize
      // smart this up
      // problem: Quant("moderate to heavy", None) considered the same as Quant("heavy", none)
      // MAYBE merge them...? here maybe no bc string overlap... keep superset/longest
      // BUT: what about "persistent and heavy seasonal rainfall" -- Quant("persistent", None),  Quant("heavy", none)
      // want merged -> Quant("persistent", None), Quant("heavy", None) ??

      // may be helpful
      //tb.newWithAttachment()

      (mention, (attachArgumentsSz + modSize + numArgs)) // The size of a mention is the sum of i) how many attachments are present ii) sum of args in each of the attachments iii) if (EventMention) ==>then include size of arguments
    }

    val maxModAttachSz = mention_attachmentSz.map(_._2).max
    val filteredMentions = mention_attachmentSz.filter(m => m._2 == maxModAttachSz).map(_._1)
    filteredMentions
  }

  // remove incomplete EVENT Mentions
  def keepMostCompleteEvents(ms: Seq[Mention], state: State): Seq[Mention] = {

    val (events, nonEvents) = ms.partition(_.isInstanceOf[EventMention])
    val (textBounds, relationMentions) = nonEvents.partition(_.isInstanceOf[TextBoundMention])
    // remove incomplete entities (i.e. under specified when more fully specified exists)

    val tbMentionGroupings =
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, m.label, m.sentence))
    // remove incomplete mentions
    val completeTBMentions =
      for ((k, tbms) <- tbMentionGroupings) yield {
//        val maxModSize: Int = tbms.map(tbm => tbm.attachments.size).max
//        val filteredTBMs = tbms.filter(m => m.attachments.size == maxModSize)
        val filteredTBMs = customAttachmentFilter(tbms)

        filteredTBMs.head
      }

    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    def argTokenInterval(m: EventMention): Interval = {
      val min =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.start).toList.min
      val max =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.end).toList.max
      Interval(start = min, end = max)
    }

    val eventMentionGroupings =
      events.map(_.asInstanceOf[EventMention]).groupBy(m => (m.label, argTokenInterval(m), m.sentence))

    // remove incomplete mentions
    val completeEventMentions =
      for ((_, ems) <- eventMentionGroupings) yield {
        // max number of arguments
        val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        // max number of argument modifications
        // todo not all attachments are equal
//        val maxArgMods = ems.map(em => em.arguments.values.flatten.map(arg => arg.attachments.size).sum).max
//        val maxModSize: Int = ems.map(em => em.arguments.values.flatMap(ms => ms.map(_.modifications.size)).max).max
//        val filteredEMs = ems.filter(m => m.arguments.values.flatten.size == maxSize &&
//          m.arguments.values.flatMap(ms => ms.map(_.attachments.size)).sum == maxArgMods)
        val filteredEMs = customAttachmentFilter(ems)
        filteredEMs.head
      }

    completeTBMentions.toSeq ++ relationMentions ++ completeEventMentions.toSeq
  }

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO Heather: write toy test for this
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  def applyAttachment(ms: Seq[Mention], state: State): Seq[Mention] = for {
    m <- ms
    //if m matches "EntityModifier"
    attachment = getAttachment(m)

    copyWithMod = m match {
      case tb: TextBoundMention => tb.copy(attachments = tb.attachments ++ Set(attachment), foundBy = s"${tb.foundBy}++mod")
      // Here, we want to keep the theme that is being modified, not the modification event itself
      case rm: RelationMention =>
        val theme = rm.arguments("theme").head.asInstanceOf[TextBoundMention]
        theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${rm.foundBy}")
      case em: EventMention =>
        val theme = em.arguments("theme").head.asInstanceOf[TextBoundMention]
        theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${em.foundBy}")
    }
  } yield copyWithMod

  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }

  def getAttachment(mention: Mention): EidosAttachment = EidosAttachment.newEidosAttachment(mention)

  // Currently used as a GLOBAL ACTION in EidosSystem:
  // Merge many Mentions of a single entity that have diff attachments, so that you have only one entity with
  // all the attachments.  Also handles filtering of attachments of the same type whose triggers are substrings
  // of each other.
  def mergeAttachments(mentions: Seq[Mention], state: State): Seq[Mention] = {
//    println("***************************")
//    println("new ROUND")
//    println("***************************")
//
//    println("Current State:")
//    state.allMentions.foreach(m => DisplayUtils.displayMention(m))
//    println("--------------------")
    // Get all the entity mentions for this span (i.e. all the "rainfall in Spain" mentions)
    val (entities, nonentities) = mentions.partition(mention => mention matches "Entity")
    val entitiesBySpan = entities.groupBy(entity => (entity.sentence, entity.tokenInterval, entity.label))
    val mergedEntities = for {
      (_, entities) <- entitiesBySpan
      // These are now for the same span, so only one should win as the main one.
      flattenedAttachments = entities.flatMap(_.attachments)
      filteredAttachments = filterAttachments(flattenedAttachments)
    } yield {
      if (filteredAttachments.nonEmpty) {
        val bestAttachment = filteredAttachments.sortWith(greaterThanOrEqual).head
        val bestEntity = entities.find(_.attachments.find(_ eq bestAttachment) != None).get

        copyWithAttachments(bestEntity, filteredAttachments)
      }
      else
        entities.head
    }
    mergedEntities.toSeq ++ nonentities
  }

  // Iteratively creates a mention which contains all of the passed in Attachments and no others
  def copyWithAttachments(mention: Mention, attachments: Seq[Attachment]): Mention = {
    // This is very inefficient, but the interface only allows for adding and subtracting one at a time.
    val attachmentless = mention.attachments.foldLeft(mention)((mention, attachment) => mention.withoutAttachment(attachment))

    attachments.foldLeft(attachmentless)((mention, attachment) => mention.withAttachment(attachment))
  }

  // Filter out substring attachments, then keep most complete.
  def filterAttachments(attachments: Seq[Attachment]) = {
    attachments
        // Perform first mapping based on class
        .groupBy(_.getClass)
        // Filter out substring attachments
        .flatMap { case (_, attachments) => filterSubstringTriggers(attachments) }
        // Next map based on both class and trigger.
        .groupBy(attachment => (attachment.getClass, triggerOf(attachment)))
        // Now that substrings are filtered, keep only most complete of each class-trigger-combo.
        .map { case (_, attachments) => filterMostComplete(attachments.toSeq) }
        .toSeq
  }

  // Keep the most complete attachment here.
  protected def filterMostComplete(attachments: Seq[Attachment]) =
      attachments.maxBy(_.asInstanceOf[EidosAttachment].argumentSize)

  // If there is a tie initially, the winner should have more arguments
  protected def lessThan(left: Attachment, right: Attachment): Boolean = {
    val triggerDiff = triggerOf(left).length - triggerOf(right).length

    if (triggerDiff != 0)
      triggerDiff < 0
    else {
      val argumentsDiff = left.asInstanceOf[EidosAttachment].argumentSize -
        right.asInstanceOf[EidosAttachment].argumentSize

      argumentsDiff < 0
    }
  }

  protected def greaterThanOrEqual(left: Attachment, right: Attachment) = !lessThan(left, right)

  // Filter out substring attachments.
  protected def filterSubstringTriggers(attachments: Seq[Attachment]): Seq[Attachment] = {

    val triggersKept = MutableSet[String]() // Cache triggers of itermediate results.

    attachments
        .sortWith(greaterThanOrEqual)
        .filter { attachment =>
          val trigger = triggerOf(attachment)

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
      case quant: Quantification => quant.quantifier
      case _ => throw new UnsupportedClassVersionError()
    }
  }
}

object EidosActions extends Actions {

  def apply(taxonomyPath: String) =
    new EidosActions(readTaxonomy(taxonomyPath))

  private def readTaxonomy(path: String): Taxonomy = {
    val input = FileUtils.getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }
}
