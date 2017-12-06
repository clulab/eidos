package org.clulab.wm

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.struct.Interval
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import utils.DisplayUtils._



// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping


case class Quantification(quantifier: String) extends Modification
case class Increase(trigger: String, quantifier: Option[Seq[String]] = None) extends Modification
case class Decrease(trigger: String, quantifier: Option[Seq[String]] = None) extends Modification


class AgroActions extends Actions with LazyLogging {

  import AgroActions._

//  def mergeMentions(mentions: Seq[Mention], state: State): Seq[Mention] = {
//    // Use label for the
//    val relabeled = copyLabel(mentions, state)
//    val clipped = clipSpan()
//  }



  /**
    * @author Gus Hahn-Powell
    * Copies the label of the lowest overlapping entity in the taxonomy
    */
  def copyLabel(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- mentions
    hypernym = m.label // we're looking for a hyponym of this guy
    hyponyms = taxonomy.hyponymsFor("Param").toSet

    if m matches "Param"

    // Get all mentions for the same span as this Param mention (that also have a Param label)
    overlapping = state.mentionsFor(m.sentence, m.tokenInterval, "Param")
  } yield {
//    println("hypernym: " + hypernym)
//    println("hyponyms: " + hyponyms.mkString(", "))
//    println("hypernyms for: " + taxonomy.hypernymsFor(hypernym))

    // assumes this is a TextBoundMention
//    val tb = m.asInstanceOf[TextBoundMention]

    // Take those other mentions, keep only other types of Params (i.e. hyponyms)
    val overlappingHyponyms = overlapping
      .filter(other => hyponyms.contains(other.label))
      .filter(other => !other.foundBy.contains(AgroSystem.EXPAND_SUFFIX))
      .toVector
//    println(s"for current mention (${tb.text}, ${tb.label} --  overlappingHyponyms:")
//    overlappingHyponyms.foreach(ov => println(s"${ov.text} [${ov.label}]"))

    // FIXME: You guys need to decide how you want to deal with "pollution of water bodies", "government subsidy on fertilizers", etc.
    // Perhaps choose the hyponym containing the head word of mention m?
    val hyponymLabels = overlappingHyponyms.map(_.label).toSet
    val hyponym = if (hyponymLabels.size > 1) {
//      println(s"${overlappingHyponyms.size} overlapping hyponym mentions found for mention '${m.text}' with label '${m.label}'")
      val synHead = m.synHead
      if (synHead.isDefined) {
        val overlappingHeads = overlappingHyponyms.filter(_.tokenInterval.contains(synHead.get))
//        overlappingHeads.foreach(oh => println(s" - overlappingWithSynHead: ${oh.text}, ${oh.label}"))
        if (overlappingHeads.nonEmpty) {
//          println(s" * Found synHead, returning label: ${overlappingHeads.head.label}")
          overlappingHeads.head
        } else {
          rightMost(overlappingHyponyms)
        }
      } else {
        rightMost(overlappingHyponyms)
      }
    } else overlappingHyponyms.head
    // this is the more specific label (ex. Rain instead of Param)
    val intervalToUse = if (m.entities.get.head.contains("IncDec")) Interval(m.tokenInterval.start+1, m.tokenInterval.end) else m.tokenInterval
    val copiedOut = m match {
      case tb: TextBoundMention => tb.copy(labels = hyponym.labels, foundBy = s"${m.foundBy}-copyLabel", tokenInterval = intervalToUse)
      case rm: RelationMention => rm.copy(labels = hyponym.labels, foundBy = s"${m.foundBy}-copyLabel", tokenInterval = intervalToUse)
      case em: EventMention => em.copy(labels = hyponym.labels, foundBy = s"${m.foundBy}-copyLabel", tokenInterval = intervalToUse)
      case _ => throw new RuntimeException("Invalid Mention Type")
    }
    copiedOut
//    clipSpan(copiedOut, state)
  }

  def rightMost(overlappingHyponyms: Seq[Mention]): Mention = {
    val sorted = overlappingHyponyms.sortBy(- _.tokenInterval.end)
    val rightMost = sorted.head
//    println(s"Using rightmost: ${rightMost.label}")
    rightMost
  }

  //TODO: modify this to not clip things like "agricultural (JJ) production",
  //TODO: but we do want to clip like "vulerablue to climate change" --> "climate change"
  def clipSpan(m: Mention, state: State): Mention = {
//    logger.debug(s"****************\nBefore clipping: (${m.words.slice(m.tokenInterval.start, m.tokenInterval.end)}")
    val start = m.tokenInterval.start
    val end = m.tokenInterval.end
    val tags = m.document.sentences(m.sentence).tags.get.slice(start, end)
    val indexedTags = m.document.sentences(m.sentence).tags.get.zipWithIndex.slice(start, end)
    val firstNNInSlice = indexedTags.indexWhere(tag => tag._1.startsWith("NN") || tag._1.startsWith("DT") || tag._1.startsWith("JJ"))
    val lastNNInSlice = indexedTags.lastIndexWhere(tag => tag._1.startsWith("NN"))
    if (firstNNInSlice == -1 || lastNNInSlice == -1) {
      return m
    }
    val tb = m.asInstanceOf[TextBoundMention]
    val interval = Interval(indexedTags(firstNNInSlice)._2, indexedTags(lastNNInSlice)._2 + 1)
//    logger.debug(s"NEW INTERVAL: (${m.words.slice(m.tokenInterval.start, m.tokenInterval.end)})\n*****************************")
    tb.copy(tokenInterval = interval)
  }




  /**
    * @author Gus Hahn-Powell
    * Keeps longest mention in each set of overlapping mentions with the same label.
    */
  def keepLongestMentions(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- mentions
    // find overlapping mentions that share the same label
    overlapping = state.mentionsFor(m.sentence, m.tokenInterval, m.label)
  } yield overlapping.maxBy(_.tokenInterval.length)

  def keepLongestMentions2(mentions: Seq[Mention], state: State): Seq[Mention] = {
    for {
      m <- mentions
      // find overlapping mentions that share the same label
      overlapping = state.mentionsFor(m.sentence, m.tokenInterval, m.label)

      filtered = overlapping.filter { overlappingMention =>
        argumentsToString(overlappingMention) == argumentsToString(m)}
//        val args = overlappingMention.arguments.values.toSeq.flatten.map(arg => arg.synHead).filter(_.isDefined).map(_.get)
//        println("synHeads: " + args.mkString(", "))
//        args.contains(m.synHead.get)
//      }
        //        for {
//          k <- args.
//        }
//        overlappingMention.synHead
//        // check to see if the arguments
//        overlappingMention.arguments == m.arguments}
    } yield {
      displayMention(m)
//      println("m.synHead: " + m.synHead.get)
      println("m args keys: " + m.arguments.keySet.mkString(", "))
      println("overlapping mentions keys: \n\t" + overlapping.foreach(ov => println(argumentsToString(ov) + "\n\t")))
      println("overlapping args keys: " + overlapping.foreach(ov => println(ov.arguments.keySet.mkString(", "))))
      println("filtered args keys: " + filtered.foreach(ov => println(ov.arguments.keySet.mkString(", "))))
      if (filtered.nonEmpty) filtered.maxBy(_.tokenInterval.length) else m
    }
  }

  def keepLongest3(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val grouped = mentions.groupBy(m => (m.sentence, m.label))
    for (k <- grouped.keys) {
      val mentionsOfType = grouped(k)

    }
    ???
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
        val maxModSize: Int = tbms.map(tbm => tbm.modifications.size).max
        val filteredTBMs = tbms.filter(m => m.modifications.size == maxModSize)
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
        val maxModSize: Int = ems.map(em => em.arguments.values.flatMap(ms => ms.map(_.modifications.size)).max).max
        val filteredEMs = ems.filter(m => m.arguments.values.flatten.size == maxSize &&
          m.arguments.values.flatMap(ms => ms.map(_.modifications.size)).max == maxModSize)
        filteredEMs
      }

    completeTBMentions.flatten.toSeq ++ relationMentions ++ completeEventMentions.flatten.toSeq
  }

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO Heather: write toy test for this
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  def applyModification(ms: Seq[Mention], state: State): Seq[Mention] = for {
    m <- ms
    //if m matches "EntityModifier"
    modification = getModification(m)
    copyWithMod = m match {
      case tb: TextBoundMention => tb.copy(modifications = tb.modifications ++ Set(modification))
      // Here, we want to keep the theme that is being modified, not the modification event itself
      case rm: RelationMention =>
        val theme = rm.arguments("theme").head.asInstanceOf[TextBoundMention]
        theme.copy(modifications = theme.modifications ++ Set(modification))
      case em: EventMention =>
        val theme = em.arguments("theme").head.asInstanceOf[TextBoundMention]
        theme.copy(modifications = theme.modifications ++ Set(modification))
    }
  } yield copyWithMod


  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }


  def getModification(m: Mention): Modification = {
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
    val source = if (url == null) scala.io.Source.fromFile(path) else scala.io.Source.fromURL(url)
    val input = source.mkString
    source.close()
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }

}