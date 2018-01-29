package org.clulab.wm

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.Aliases.Quantifier
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.BufferedSource


// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping


case class Quantification(quantifier: Quantifier) extends Attachment
case class Increase(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends Attachment
case class Decrease(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends Attachment


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
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, reduceNounPhrases(m.label)))

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
      for ((_, ems) <- eventMentionGroupings) yield {
        // max number of arguments
        val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        // max number of argument modifications
        val maxArgMods = ems.map(em => em.arguments.values.flatten.map(arg => arg.attachments.size).sum).max
//        val maxModSize: Int = ems.map(em => em.arguments.values.flatMap(ms => ms.map(_.modifications.size)).max).max
        val filteredEMs = ems.filter(m => m.arguments.values.flatten.size == maxSize &&
          m.arguments.values.flatMap(ms => ms.map(_.attachments.size)).sum == maxArgMods)
        filteredEMs
      }

    completeTBMentions.flatten.toSeq ++ relationMentions ++ completeEventMentions.flatten.toSeq
  }

  // Use to temporarily collapse noun phrase types for grouping
  def reduceNounPhrases(s:String) = s match {
    case "NounPhrase-Inc" => "NounPhrase"
    case "NounPhrase-Dec" => "NounPhrase"
    case "NounPhrase-Quant" => "NounPhrase"
    case _ => s
  }

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO Heather: write toy test for this
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  def applyAttachment(ms: Seq[Mention], state: State): Seq[Mention] = for {
    m <- ms
    //if m matches "EntityModifier"
    (attachment, npLabel) = getAttachment(m)

    copyWithMod = m match {
      case tb: TextBoundMention =>
        val copyLabels = Seq(npLabel) ++ m.labels
        tb.copy(attachments = tb.attachments ++ Set(attachment), foundBy = s"${tb.foundBy}++mod", labels = copyLabels)
      // Here, we want to keep the theme that is being modified, not the modification event itself
      case rm: RelationMention =>
        val theme = rm.arguments("theme").head.asInstanceOf[TextBoundMention]
        val copyLabels = Seq(npLabel) ++ theme.labels
        theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${rm.foundBy}", labels = copyLabels)
      case em: EventMention =>
        val theme = em.arguments("theme").head.asInstanceOf[TextBoundMention]
        val copyLabels = Seq(npLabel) ++ theme.labels
        theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${em.foundBy}", labels = copyLabels)
    }
  } yield copyWithMod



  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }


  def getAttachment(m: Mention): (Attachment, String) = {
    m.label match {
      case "Quantification" => {
        val quantifier = m.asInstanceOf[EventMention].trigger.text
        (new Quantification(quantifier), NP_QUANT_LABEL)
      }
      case "Increase" => {
        val quantifiers = getOptionalQuantifiers(m)
        val trigger = m.asInstanceOf[EventMention].trigger.text
        (new Increase(trigger, quantifiers), NP_INC_LABEL)
      }
      case "Decrease" => {
        val quantifiers = getOptionalQuantifiers(m)
        val trigger = m.asInstanceOf[EventMention].trigger.text
        //println(s"Decrease found: ${new Decrease(trigger, quantifiers)}")
        (new Decrease(trigger, quantifiers), NP_DEC_LABEL)
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

object AgroActions extends Actions {

  val taxonomy = readTaxonomy("org/clulab/wm/grammars/taxonomy.yml")
  val NP_INC_LABEL = "NounPhrase-Inc"
  val NP_DEC_LABEL = "NounPhrase-Dec"
  val NP_QUANT_LABEL = "NounPhrase-Quant"

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