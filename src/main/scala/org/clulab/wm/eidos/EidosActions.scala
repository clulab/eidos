package org.clulab.wm.eidos

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.eidos.Aliases.Quantifier
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.BufferedSource


// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping


case class Quantification(quantifier: Quantifier, adverbs: Option[Seq[String]]) extends Attachment
case class Increase(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends Attachment
case class Decrease(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends Attachment


class EidosActions extends Actions with LazyLogging {

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

      val size = if(mention.isInstanceOf[EventMention])
                    (mention.arguments.values.flatten.size * 100)
                  else
                    0

      val modSize = if (mention.isInstanceOf[TextBoundMention])
                        (mention.attachments.size * 10)
                    else if (mention.isInstanceOf[EventMention])
                        (mention.asInstanceOf[EventMention].arguments.values.flatten.map(arg => arg.attachments.size).sum * 10)
                    else
                        0

      val attachmentsList = if (mention.isInstanceOf[TextBoundMention])
                              Some(mention.attachments)
                            else if (mention.isInstanceOf[EventMention])
                              Some(mention.asInstanceOf[EventMention].arguments.values.flatten.map(m => m.attachments).flatten.toSet)
                            else
                              None

      val attachArgumentsSz =  (for (attachment <- attachmentsList.get) yield {
        val attachmentArgSz = attachment match {
          case quant: Quantification => if (quant.adverbs.isDefined) quant.adverbs.get.size else 0
          case inc: Increase => if (inc.quantifier.isDefined) inc.quantifier.get.size else 0
          case dec: Decrease => if (dec.quantifier.isDefined) dec.quantifier.get.size else 0
          case _ => 0
        }
        attachmentArgSz
      }).sum

      (mention, (attachArgumentsSz + modSize + size)) // The size of a mention is the sum of i) how many attachments are present ii) sum of args in each of the attachments iii) if (EventMention) ==>then include size of arguments
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
        filteredTBMs
      }

    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    val eventMentionGroupings =
      events.map(_.asInstanceOf[EventMention]).groupBy(m => (m.trigger, m.label, m.tokenInterval))

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
        filteredEMs
      }

    completeTBMentions.flatten.toSeq ++ relationMentions ++ completeEventMentions.flatten.toSeq
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


  def getAttachment(m: Mention): Attachment = {
    m.label match {
      case "Quantification" => {
        val quantifier = m.asInstanceOf[EventMention].trigger.text
        val adverbs = m.asInstanceOf[EventMention].arguments.get("adverb") match {
          case Some(found) => Some(found.map(_.text))
          case None => None
        }
        new Quantification(quantifier, adverbs)
      }
      case "Increase" => {
        val quantifiers = getOptionalQuantifiers(m)
        val trigger = m.asInstanceOf[EventMention].trigger.text
        new Increase(trigger, quantifiers)
      }
      case "Decrease" => {
        val quantifiers = getOptionalQuantifiers(m)
        val trigger = m.asInstanceOf[EventMention].trigger.text

        //println(s"Decrease found: ${new Decrease(trigger, quantifiers)}")
        new Decrease(trigger, quantifiers)
      }
    }
  }


  def getOptionalQuantifiers(m: Mention): Option[Seq[Quantifier]] = {
    m.asInstanceOf[EventMention]
      .arguments
      .get("quantifier")
      .map(qs => qs.map(_.text))
  }


}

object EidosActions extends Actions {

  val taxonomy = readTaxonomy("org/clulab/wm/grammars/taxonomy.yml")

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
