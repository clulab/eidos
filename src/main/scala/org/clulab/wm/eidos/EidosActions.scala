package org.clulab.wm.eidos

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.eidos.attachments._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

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

    val mention_attachmentSz = for (mention <- mentions) yield {

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

      val attachArgumentsSz = attachmentsSet.toSeq.map(_.asInstanceOf[EidosAttachment].argumentSize).sum

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
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, m.label))

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
    val eventMentionGroupings =
      events.map(_.asInstanceOf[EventMention]).groupBy(m => (m.label, m.tokenInterval))

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
}

object EidosActions extends Actions {

  def apply(taxonomyPath: String) =
    new EidosActions(readTaxonomy(taxonomyPath))

  private def readTaxonomy(path: String): Taxonomy = {
    val url = getClass.getClassLoader.getResource(path)
    val source:BufferedSource = if (url == null) scala.io.Source.fromFile(new File(path)) else scala.io.Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }
}
