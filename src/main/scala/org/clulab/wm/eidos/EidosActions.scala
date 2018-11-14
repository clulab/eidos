package org.clulab.wm.eidos

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.{FileUtils, MentionUtils}
import org.clulab.struct.Interval
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import org.clulab.wm.eidos.actions.{AttachmentHandler, EidosBaseActions}
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.document.TimeInterval
import org.clulab.wm.eidos.entities.ExpansionHandler

import scala.collection.mutable.{ArrayBuffer, Set => MutableSet}

// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping

class EidosActions(val taxonomy: Taxonomy, val expansionHandler: ExpansionHandler) extends EidosBaseActions with LazyLogging {

  /*
      Global Action -- performed after each round in Odin
  */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = {

    // expand arguments
    val (expandable, textBounds) = mentions.partition(m => EidosSystem.CAG_EDGES.contains(m.label))
    val expanded = expansionHandler.expandArguments(expandable, state)
    val mostComplete = keepMostCompleteEvents(expanded, state.updated(expanded))
    val result = mostComplete ++ textBounds

    // Merge attachments
    val merged = mergeAttachments(result, state.updated(result))

    // If the cause of an event is itself another event, replace it with the nested event's effect
    // collect all effects from causal events
    val (causal, nonCausal) = merged.partition(m => EidosSystem.CAG_EDGES.contains(m.label))

    val assemble1 = createEventChain(causal, "effect", "cause")
    // FIXME please
    //val assemble2 = createEventChain(assemble1, "cause", "effect")
    val assemble2 = assemble1
    // FIXME
    // in the sentence below we stitch together the sequence of cause->effect events
    // but some expanded nounphrase remains, which shouldn't be displayed in the webapp
    // In Kenya , the shortened length of the main growing season , due in part to a delayed onset of seasonal rainfall , coupled with long dry spells and below-average rainfall is resulting in below-average production prospects in large parts of the eastern , central , and southern Rift Valley .
    val modifiedMentions = assemble2 ++ nonCausal

    // Basic coreference
    val afterResolving = basicDeterminerCoref(modifiedMentions, state)

    // I know I'm an unnecessary line of code, but I am useful for debugging and there are a couple of things left to debug...
    afterResolving
  }

  def basicDeterminerCoref(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val (eventMentions, otherMentions) = mentions.partition(_.isInstanceOf[EventMention])
    if (eventMentions.isEmpty) return mentions

    val orderedBySentence = eventMentions.groupBy(_.sentence)
    val numSentences = eventMentions.head.document.sentences.length
    if (orderedBySentence.isEmpty) {
      return mentions
    } else {
      val resolvedMentions = new ArrayBuffer[Mention]
      for (i <- 1 until numSentences) {

        for (mention <- orderedBySentence.getOrElse(i, Seq.empty[Mention])) {

          // If there is an event with "this/that" as cause...
          if (EidosActions.existsDeterminerCause(mention)) {

            // Get Causal mentions from the previous sentence (if any)
            val prevSentenceCausal = getPreviousSentenceCausal(orderedBySentence, i)
            if (prevSentenceCausal.nonEmpty) {

              // If there was also a causal event in the previous sentence
              val lastOccurring = prevSentenceCausal.sortBy(-_.tokenInterval.end).head
              // antecedant
              val prevEffects = lastOccurring.arguments("effect")
              // reference
              val currCauses = mention.asInstanceOf[EventMention].arguments("cause")
              if (prevEffects.nonEmpty && currCauses.nonEmpty) {
                // todo: cover if there is more than one effect?
                val antecedent = prevEffects.head
                val anaphor = currCauses.head
                // Make a new CrossSentence mention using the previous effect as the anchor
                // Note: Overly simplistic, this is a first pass
                // todo: expand approach
                val corefMention = new CrossSentenceMention(
                  labels = taxonomy.hypernymsFor(EidosSystem.COREF_LABEL),
                  anchor = antecedent,
                  neighbor = anaphor,
                  arguments = Map[String, Seq[Mention]]((EidosActions.ANTECEDENT, Seq(antecedent)), (EidosActions.ANAPHOR, Seq(anaphor))),
                  document = mention.document,
                  keep = true,
                  foundBy = s"BasicCorefAction_ant:${lastOccurring.foundBy}_ana:${mention.foundBy}",
                  attachments = Set.empty[Attachment]
                )
                resolvedMentions.append(corefMention)

              } else throw new RuntimeException(s"Previous or current Causal mention doesn't have effects " +
                s"\tsent1: ${lastOccurring.sentenceObj.getSentenceText}\n" +
                s"\tsent2 ${mention.sentenceObj.getSentenceText}")
            }
          }
        }
      }
      mentions ++ resolvedMentions
    }
  }

  def getPreviousSentenceCausal(orderedBySentence: Map[Int, Seq[Mention]], i: Int): Seq[Mention] = {
    val prevSentenceMentions = orderedBySentence.getOrElse(i - 1, Seq.empty[Mention])
    prevSentenceMentions.filter(_ matches EidosSystem.CAUSAL_LABEL)
  }


  def createEventChain(causal: Seq[Mention], arg1: String, arg2: String): Seq[Mention] = {
    val arg1Mentions = State(causal.flatMap(_.arguments.getOrElse(arg1, Nil)))
    // replace event causes with captured effects if possible
    // to stitch together sequences of causal events
    val assembled = for {
      m <- causal
      _ = require(m.arguments(arg2).length == 1, "we only support a single cause and effect per event")
      arg2Mention <- m.arguments.getOrElse(arg2, Nil)
    } yield {
      if (m.paths.isEmpty) {
        // if the mention was captured by a surface rule then there is no syntactic path to retrieve
        // return mention as-is
        Seq(m)
      } else {
        // odin mentions keep track of the path between the trigger and the argument
        // below we assume there is only one cause arg, so beware (see require statement abov)
        val landed = m.paths(arg2)(m.arguments(arg2).head).last._2 // when the rule matched, it landed on this
        // when the rule matched, it landed on this
        // println("---------")
        // println(s"rule: ${m.foundBy}")
        // println(s"cause: ${m.arguments(arg2).head.text}")
        // m.paths(arg2).keys.foreach(x => println(s"${x.text} - ${x.foundBy} - ${x.start} ${x.end}"))
        // println("---------")
        assembleEventChain(m.asInstanceOf[EventMention], arg2Mention, landed, arg1Mentions)
      }
    }

    assembled.flatten
  }

  /*
      Method for assembling Event Chain Events from flat text, i.e., ("(A --> B)" --> C) ==> (A --> B), (B --> C)
  */

  // this method ensures that if a cause of one event is the effect of another then they are connected
  // FIXME -- not working for this:
  //    However , prospects for 2017 aggregate cereal production are generally unfavourable as agricultural activities continue to be severely affected by the protracted and widespread insecurity , which is constraining farmers ' access to fields and is causing large scale displacement of people , input shortages and damage to households ' productive assets .
  def assembleEventChain(event: EventMention, origCause: Mention, landed: Int, effects: State): Seq[EventMention] = {
    effects.mentionsFor(event.sentence, landed) match {
      case Seq() => Seq(event) // return original event
      case newCauses =>
        val arguments = event.arguments
        newCauses.map { nc =>
          val retainedCauses = arguments("cause").filterNot(_ == origCause)
          val newArguments = arguments.filterKeys(_ != "cause") + (("cause", retainedCauses :+ nc))
          val mentions = (newArguments.values.flatten.toSeq :+ event.trigger)
          val newStart = mentions.map(_.start).min
          val newEnd = mentions.map(_.end).max
          event.copy(arguments = newArguments, tokenInterval = Interval(newStart, newEnd))
        }
    }
  }

  /*
    Mention State / Attachment Methods
  */

  def applyTimeAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for {
      m <- ms
      trigger = m.asInstanceOf[EventMention].trigger
      theme = AttachmentHandler.tieBreaker(m.arguments("theme")).asInstanceOf[TextBoundMention]
      time: Option[TimeInterval] = m.document.asInstanceOf[EidosDocument].times(m.sentence).filter(_.span._1 == trigger.startOffset).headOption
    } yield time match {
      case None => theme
      case Some(t) => theme.withAttachment(new Time(t))
    }
  }


  def applyLocationAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for {
      m <- ms
      trigger = m.asInstanceOf[EventMention].trigger
      theme = AttachmentHandler.tieBreaker(m.arguments("theme")).asInstanceOf[TextBoundMention]
      // time: Option[TimeInterval] = m.document.asInstanceOf[EidosDocument].times(m.sentence).filter(_.span._1 == trigger.startOffset).headOption
      location: Option[GeoPhraseID] = m.document.asInstanceOf[EidosDocument].geolocs(m.sentence).filter(_.StartOffset_locs == trigger.startOffset).headOption

    } yield location match {
      case None => theme
      case Some(l) => theme.withAttachment(new Location(l))
    }
  }




  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }

  def pass(mentions: Seq[Mention], state: State): Seq[Mention] = mentions



  // Currently used as a GLOBAL ACTION in EidosSystem:
  // Merge many Mentions of a single entity that have diff attachments, so that you have only one entity with
  // all the attachments.  Also handles filtering of attachments of the same type whose triggers are substrings
  // of each other.
  def mergeAttachments(mentions: Seq[Mention], state: State): Seq[Mention] = {

    // Get all the entity mentions for this span (i.e. all the "rainfall in Spain" mentions)
    val (entities, nonentities) = mentions.partition(mention => mention matches "Entity")
    val entitiesBySpan = entities.groupBy(entity => (entity.sentence, entity.tokenInterval, entity.label))
    val mergedEntities = for {
      (_, entities) <- entitiesBySpan
      // These are now for the same span, so only one should win as the main one.
      flattenedTriggeredAttachments = entities.flatMap(_.attachments.filter(_.isInstanceOf[TriggeredAttachment]).map(_.asInstanceOf[TriggeredAttachment]))
      flattenedContextAttachments = entities.flatMap(_.attachments.filter(_.isInstanceOf[ContextAttachment]).map(_.asInstanceOf[ContextAttachment]))
      filteredAttachments = filterAttachments(flattenedTriggeredAttachments)
    } yield {
      if (filteredAttachments.nonEmpty) {

        val bestAttachment = filteredAttachments.sorted.reverse.head
        // Since head was used above and there could have been a tie, == should be used below
        // The tie can be broken afterwards.
        val bestEntities = entities.filter(_.attachments.exists(_ == bestAttachment))
        val bestEntity = AttachmentHandler.tieBreaker(bestEntities)

        MentionUtils.withOnlyAttachments(bestEntity, filteredAttachments  ++ flattenedContextAttachments)
      }
      else
        AttachmentHandler.tieBreaker(entities)
    }

    val res = keepMostCompleteEvents(mergedEntities.toSeq ++ nonentities, state)
    res
  }

  // Filter out substring attachments, then keep most complete.
  def filterAttachments(attachments: Seq[TriggeredAttachment]): Seq[TriggeredAttachment] = {
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
  protected def filterMostComplete(attachments: Seq[TriggeredAttachment]): TriggeredAttachment =
      attachments.maxBy(_.argumentSize)

  // Filter out substring attachments.
  protected def filterSubstringTriggers(attachments: Seq[TriggeredAttachment]): Seq[TriggeredAttachment] = {
    val triggersKept = MutableSet[String]() // Cache triggers of itermediate results.

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

object EidosActions extends Actions {

  // Used for simplistic coreference identification
  val COREF_DETERMINERS: Set[String] = Set("this", "that", "these", "those")
  val ANTECEDENT: String = "antecedent"
  val ANAPHOR: String = "anaphor"

  


  def apply(taxonomyPath: String, expansionHandler: ExpansionHandler) =
      new EidosActions(readTaxonomy(taxonomyPath), expansionHandler)

  private def readTaxonomy(path: String): Taxonomy = {
    val input = FileUtils.getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }

  def existsDeterminerCause(mention: Mention): Boolean = {
    startsWithCorefDeterminer(mention.arguments("cause").head)
  }

  def startsWithCorefDeterminer(m: Mention): Boolean = {
    val corefDeterminers = COREF_DETERMINERS
    corefDeterminers.exists(det => m.text.toLowerCase.startsWith(det))
  }

}
