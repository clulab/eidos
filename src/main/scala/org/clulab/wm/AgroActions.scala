package org.clulab.wm

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.BufferedSource


// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping


case class Quantification(quantifier: String) extends Attachment
case class Increase(trigger: String, quantifier: Option[Seq[String]] = None) extends Attachment
case class Decrease(trigger: String, quantifier: Option[Seq[String]] = None) extends Attachment


class AgroActions extends Actions with LazyLogging {

  import AgroActions._

  /**
    * @author Gus Hahn-Powell
    * Copies the label of the lowest overlapping entity in the taxonomy
    */


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
        val maxModSize: Int = tbms.map(tbm => tbm.attachments.size).max
        val filteredTBMs = tbms.filter(m => m.attachments.size == maxModSize)
        filteredTBMs
      }

    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    val eventMentionGroupings =
      events.map(_.asInstanceOf[EventMention]).groupBy(m => (m.trigger, m.label))

    // remove incomplete mentions
    val completeEventMentions =
      for ((k, ems) <- eventMentionGroupings) yield {
        val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        val maxModSize: Int = ems.map(em => em.arguments.values.flatMap(ms => ms.map(_.attachments.size)).max).max
        val filteredEMs = ems.filter(m => m.arguments.values.flatten.size == maxSize &&
          m.arguments.values.flatMap(ms => ms.map(_.attachments.size)).max == maxModSize)
        filteredEMs
      }

    completeTBMentions.flatten.toSeq ++ relationMentions ++ completeEventMentions.flatten.toSeq
  }

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO Heather: write toy test for this
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  def applyAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    val out = for {
    // group mentions by their entities (group all mentions for same entitiy together)
      (_, mentionsOfThisEntity) <- ms.groupBy(m => (m.label, m.tokenInterval, m.sentence))
      // get all attachments for this entity
      allmodsOfThisEntity = mentionsOfThisEntity.map(mention => getAttachment(mention)).toSet
      // Get the entity to use for copying
      m = mentionsOfThisEntity.head
      copyWithMod = m match {
        case tb: TextBoundMention => tb.copy(attachments = tb.attachments ++ allmodsOfThisEntity)
        // Here, we want to keep the theme that is being modified, not the attachment event itself
        case rm: RelationMention =>
          val theme = rm.arguments("theme").head.asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ allmodsOfThisEntity)
        case em: EventMention =>
          val theme = em.arguments("theme").head.asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ allmodsOfThisEntity)
      }
    } yield copyWithMod
    out.toSeq
  }


  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }


  def getAttachment(m: Mention): Attachment = {
    m.label match {
      case "Quantification" => {
        val quantifier = m.asInstanceOf[EventMention].trigger.text
        new Quantification(quantifier)
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


  def getOptionalQuantifiers(m: Mention): Option[Seq[String]] = {
    m.asInstanceOf[EventMention]
      .arguments
      .get("quantifier")
      .map(qs => qs.map(_.text))
  }


}

object AgroActions extends Actions {

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