package org.clulab.wm.eidos

import java.util.regex.Pattern

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.MentionUtils
import org.clulab.struct.Interval
import org.clulab.wm.eidos.actions.{CorefHandler, MigrationUtils}
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.matching.Regex
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.odin.EventMention
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.attachments.CountUnit._
import org.clulab.wm.eidos.attachments.CountModifier._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Set => MutableSet}
import scala.util.Try

// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping

class EidosActions(val expansionHandler: Option[Expander], val coref: Option[CorefHandler]) extends Actions with LazyLogging {

  /*
      Global Action -- performed after each round in Odin
  */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // Expand mentions, if enabled
    val expanded = expansionHandler.map(_.expand(mentions, state)).getOrElse(mentions)
    // Merge attachments
    val merged = mergeAttachments(expanded, state.updated(expanded))
    // Keep only the most complete version of any given Mention
    val mostComplete = keepMostCompleteEvents(merged, state.updated(merged))
    // If the cause of an event is itself another event, replace it with the nested event's effect
    // collect all effects from causal events
    val (causal, nonCausal) = merged.partition(m => EidosSystem.CAG_EDGES.contains(m.label))

    val assemble1 = createEventChain(causal, "effect", "cause")
    // FIXME please
    //val assemble2 = createEventChain(assemble1, "cause", "effect")
    // in the sentence below we stitch together the sequence of cause->effect events
    // but some expanded nounphrase remains, which shouldn't be displayed in the webapp
    // In Kenya , the shortened length of the main growing season , due in part to a delayed onset of seasonal rainfall , coupled with long dry spells and below-average rainfall is resulting in below-average production prospects in large parts of the eastern , central , and southern Rift Valley .
    val modifiedMentions = assemble1 ++ nonCausal

    // Basic coreference, if enabled
    val afterResolving = coref.map(_.resolveCoref(modifiedMentions, state)).getOrElse(modifiedMentions)

    // I know I'm an unnecessary line of code, but I am useful for debugging and there are a couple of things left to debug...
    afterResolving
  }


  /**
    * Normalizes migration events, e.g., by extracting the actual number of people displaced from the "group" argument
    * @param mentions Sequence of mentions to be normalized
    * @param state unused
    * @return the sequence of normalized mentions
    */
  def normalizeGroup(mentions: Seq[Mention], state: State): Seq[Mention] = {
    type Provenance = (String, Int, Int) // text, startOffset, endOffset

    //val pattern = """[0-9]+,?[0-9]+""".r
    val normalized = new ArrayBuffer[Mention]()

    for (m <- mentions) {
      // we only care about migration events here
      if (m.isInstanceOf[EventMention] && m.label == "HumanMigration") {
        val em = m.asInstanceOf[EventMention]

        var countAttachments = new mutable.HashSet[EidosAttachment]()
        if (em.arguments.contains("group")) {
          val ga = em.arguments("group").head // there should be a single group; this should be safe
          val na:Option[(Int, Int, Double)] = extractNumber(ga.sentenceObj, ga.start, ga.end)
          if (na.nonEmpty) {
            // the event text includes +/- 1 words outside of the event span, to make sure it captures all modifiers
            val startWordIndex = math.max(0, em.start - 1)
            val endWordIndex = math.min(em.end + 1, em.sentenceObj.size)
            // TODO This strategy is worrisome.
            // Later searching for regular expressions in this eventText can be problematic, because
            // offsets into the event text constructed with mkString aren't necessarily offsets into
            // the original text which may have included tokens separated by more than one space, as
            // is often the case with punctuation, or tokens that have been expanded from contractions
            // or from Greek symbols.
            // The regular expressions do not check for matches against whole words.  So,
            // "The hat least worn" will match "at least".  Are there problems with case as well?
            // It seems like this might be the case for matching a subsequence of words against the
            // entire em.sentenceObj.words.  This might address both issues.
            val eventText = em.sentenceObj.words.slice(startWordIndex, endWordIndex).mkString(" ")
            val eventStartOffset = em.sentenceObj.startOffsets(startWordIndex)

            //
            // compute the actual count
            //
            // double, text, startOffset, endOffset
            val count: (Double, Option[Provenance]) = (
              na.get._3,
              Some(
                ga.sentenceObj.words.slice(na.get._1, na.get._2).mkString(" "),
                ga.sentenceObj.startOffsets(na.get._1),
                ga.sentenceObj.endOffsets(na.get._2 - 1)
              )
            )
            //logger.debug(s"FOUND NUMBER: $count")

            def newProvenance(regexMatch: Regex.Match): Provenance =
                (regexMatch.matched, regexMatch.start + eventStartOffset, regexMatch.end + eventStartOffset)

            //
            // search for the measurement unit in the *whole* event span
            //
            // countUnit, Option(text, startOffset, endOffset)
            val countUnit: (CountUnit.Value, Option[Provenance]) =
              if(ga.sentenceObj.entities.get(na.get._1) == "PERCENT") {
                (Percentage, Some((ga.sentenceObj.words(na.get._1), ga.sentenceObj.startOffsets(na.get._1), ga.sentenceObj.endOffsets(na.get._1))))
              } else {
                val d = """daily""".r.findFirstMatchIn(eventText)
                if(d.nonEmpty) {
                  (Daily, Some(newProvenance(d.get)))
                } else {
                  val w = """weekly""".r.findFirstMatchIn(eventText)
                  if(w.nonEmpty) {
                    (Weekly, Some(newProvenance(w.get)))
                  } else {
                    val m = """monthly""".r.findFirstMatchIn(eventText)
                    if(m.nonEmpty) {
                      (Monthly, Some(newProvenance(m.get)))
                    } else {
                      (Absolute, None)
                    }
                  }
                }
              }
            //logger.debug(s"FOUND UNIT: $countUnit")

            //
            // search for the count modifiers in the *whole* event span
            //
            // countModifier, Option(text, startOffset, endOffset)
            val countModifier: (CountModifier.Value, Option[Provenance]) = {
              val a = """about|approximately|around""".r.findFirstMatchIn(eventText)
              if(a.nonEmpty) {
                (Approximate, Some(newProvenance(a.get)))
              } else {
                val min = """(at\s+least)|(more\s+than)|over""".r.findFirstMatchIn(eventText)
                if(min.nonEmpty) {
                  (Min, Some(newProvenance(min.get)))
                } else {
                  val max = """(at\s+most)|(less\s+than)|almost|under""".r.findFirstMatchIn(eventText)
                  if(max.nonEmpty) {
                    (Max, Some(newProvenance(max.get)))
                  } else {
                    (NoModifier, None)
                  }
                }
              }
            }
            //logger.debug(s"FOUND MOD: $countModifier")

            val (text, startOffset, endOffset) = {
              def isPrefix(value: String, texts: Seq[String]): Boolean = texts.exists { text =>
                text.length > value.length && text.startsWith(value)
              }

              // This is an attempt to make the text pretty and describe the interval succinctly.
              val option: Seq[Option[Provenance]] = Seq(count._2, countModifier._2, countUnit._2)
              val some: Seq[Provenance] = option.filter(_.isDefined).map(_.get)
              val sortedByStartOffset: Seq[Provenance] = some.sortBy(_._2)
              val texts: Seq[String] = sortedByStartOffset.map(_._1)
              // Percents in particular cause repeated values like "about 75 percent 75"
              // Keep 75 percent, but rule out 75.
              val longTexts: Seq[String] = texts.filter { text => !isPrefix(text, texts) }
              val text: String = longTexts.mkString(" ")
              val startOffset: Int = sortedByStartOffset.head._2
              val sortedByEndOffset: Seq[Provenance] = some.sortBy(_._3)
              val endOffset: Int = sortedByEndOffset.last._3

              (text, startOffset, endOffset)
            }

            countAttachments +=
              new CountAttachment(
                text,
                MigrationGroupCount(count._1, countModifier._1, countUnit._1),
                startOffset, endOffset
              )
          }
        }

        //
        // TODO: need to normalize time intervals too
        //   if timeStart and timeEnd are present, reduce them to a single field that stores the interval [timeStart.start, timeEnd.end)
        //   this should be done in a different method?
        //

        if(countAttachments.nonEmpty) {
          val oldGroupArg = em.arguments("group").head //getting the group arg from the original event
          val newGroupArg = oldGroupArg.withAttachment(countAttachments.head) //copying the old one but with the newly-found attachments
          val newArgs = em.arguments ++ Map("group" -> Seq(newGroupArg)) //creating the new set of args by taking the original event arguments and adding the new group argument
          val withNewArg = MigrationUtils.copyWithNewArgs(em, newArgs, Some("withGroupNormalized")) //creating the new event argument with the new set of arguments
          normalized += withNewArg
        } else {
          normalized += em
        }
      } else {
        normalized += m
      }
    }

    normalized
  }

  def migrationActionFlow(mentions: Seq[Mention], state: State): Seq[Mention] =
      withArgs(normalizeGroup(mentions, state))

  def withArgs(mentions: Seq[Mention]): Seq[Mention] =
      mentions.filter(_.arguments.nonEmpty)

  protected def extractNumber(sentence: Sentence, startGroup: Int, endGroup: Int): Option[(Int, Int, Double)] = {
    // we need the NER tags for number extraction; these are better than regexes!
    if (sentence.entities.isEmpty)
      return None

    var start = -1
    var end = -1
    var done = false

    for(i <- startGroup until endGroup if ! done) {
      val ne = sentence.entities.get(i)
      if(ne == "NUMBER" || ne == "PERCENT") {
        if(start == -1) {
          start = i
        }
        end = i + 1
      } else if(start != -1) {
        done = true
      }
    }

    if (start != -1) {
      // ideally, get the value of this number from .norms
      // however, this fails sometimes now, because the B/I-TIME tags override this column!
      // let's try to be robust; fall back to words if needed
      val n = toNumber(sentence.norms.get(start))
      if (n.isDefined)
        return Some((start, end, n.get))
      else if (end == start + 1) {
        val n = toNumber(sentence.words(start))
        if (n.isDefined)
          return Some((start, end, n.get))
      }
    }

    // could not find a meaningful number
    None
  }

  protected def toNumber(string: String): Option[Double] = {
    val matcher = EidosActions.BAD_NUMBER_PATTERN.matcher(string)
    val cleanString = matcher.replaceAll("")

    Try(cleanString.toDouble).toOption
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
        assembleEventChain(m.asInstanceOf[EventMention], arg2Mention, landed, arg1Mentions)
      }
    }

    assembled.flatten
  }

  /*
      Filtering Methods
   */

  // This was essentially .head before, but that is dependent on order.
  protected def tieBreaker(mentions: Seq[Mention]): Mention = {
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

  def filterSubstringEntities(entities: Seq[TextBoundMention]): Seq[Mention] = {

    val entityGroups = entities.groupBy(event => (event.sentence, event.label))

    val filteredForTextSubsumption = for {
      (_, entitiesInGroup) <- entityGroups

      entitiesKept = MutableSet[TextBoundMention]() // Cache intermediate events.

      filtered = entitiesInGroup
        // most args/longest/attachiest first
        .sortBy(ent => -/*(*/mentionAttachmentWeight(ent)) //) + ent.tokenInterval.length))
        // Check to see if it's subsumed by something already there
        .filter { entity =>
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

  // True if A subsumes B
  def subsumesString(a: Set[String], b: Set[String]): Boolean = b.forall(elem => contained(elem, a))
  def contained(s: String, a: Set[String]): Boolean = a.exists(elem => elem.contains(s))
  // Interval based
  def subsumesInterval(a: Set[Interval], b: Set[Interval]): Boolean = b.forall(elem => contained(elem, a))
  def contained(s: Interval, a: Set[Interval]): Boolean = a.exists(elem => elem.contains(s))


  def filterSubstringArgumentEvents(events: Seq[EventMention]): Seq[Mention] = {

    def argumentTexts(em: EventMention): Set[String] = em.arguments.values.toSet.flatten.map(m => m.text)

    // Return true if the event subsumes all the args
    def eventArgsSubsume(argsToCheck: Set[String], event: EventMention): Boolean = {
      val eventArgStrings = argumentTexts(event)
      subsumesString(eventArgStrings, argsToCheck)
    }


    val triggerGroups = events.groupBy(event => (event.sentence, event.label, event.trigger.tokenInterval))

    val filteredForArgumentSubsumption = for {
      (_, eventsInGroup) <- triggerGroups

      eventsKept = MutableSet[EventMention]() // Cache intermediate events.

      filtered = eventsInGroup
        // most args/longest/attachiest first
        .sortBy(event => -(mentionAttachmentWeight(event) + argTokenInterval(event).length))
        // Check to see if it's subsumed by something already there
        .filter { event =>
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

  // We need to remove underspecified EventMentions of near-duplicate groupings
  // (ex. same phospho, but one is missing a site)
  def argTokenInterval(m: EventMention): Interval =  {

    if (m.arguments.keys.nonEmpty) {
    val min =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.start).toList.min
    val max =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.end).toList.max
    Interval(start = min, end = max)
    }
    else {
      logger.warn("Event with no arguments.")
      Interval(start = m.trigger.tokenInterval.start, end = m.trigger.tokenInterval.end)
    }
  }

  def importanceFromLengthAndAttachments(m: EventMention): Int = {
    //val allArgMentions = m.arguments.values.toSeq.flatten.map(mention => mention.attachments.size)
    argTokenInterval(m).length
  }

  // Remove incomplete Mentions
  def keepMostCompleteEvents(ms: Seq[Mention], state: State): Seq[Mention] = {
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

    val res = completeTBMentions.toSeq ++ relationMentions ++ completeEventMentions.toSeq

    // Useful for debug
    res
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
          val mentions = newArguments.values.flatten.toSeq :+ event.trigger
          val newStart = mentions.map(_.start).min
          val newEnd = mentions.map(_.end).max
          event.copy(arguments = newArguments, tokenInterval = Interval(newStart, newEnd))
        }
    }
  }

  /*
      Mention State / Attachment Methods
   */

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

  def applyTimeAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for (m <- ms; entity <- m.arguments("entity"); time <- m.arguments("time")) yield {
      MentionUtils.withMoreAttachments(entity, time.attachments.toSeq)
    }
  }

  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    logger.debug("DEBUG ACTION")
    ms
  }

  def getAttachment(mention: Mention): EidosAttachment = EidosAttachment.newEidosAttachment(mention)

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
      flattenedTriggeredAttachments = entities.flatMap(_.attachments.collect{ case a: TriggeredAttachment => a })
      flattenedContextAttachments = entities.flatMap(_.attachments.collect{ case a: ContextAttachment => a })
      filteredAttachments = filterAttachments(flattenedTriggeredAttachments)
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

      MentionUtils.withOnlyAttachments(tieBreaker(bestEntities), filteredAttachments ++ flattenedContextAttachments)
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
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val BAD_NUMBER_PATTERN: Pattern = "[~%,<>]".r.pattern
  // Used for simplistic coreference identification
  val COREF_DETERMINERS: Set[String] = Set("this", "that", "these", "those")
  val ANTECEDENT: String = "antecedent"
  val ANAPHOR: String = "anaphor"

  def fromConfig(config: Config): EidosActions = {
    val useCoref: Boolean = config[Boolean]("useCoref")
    val corefHandler = if (useCoref) Some(CorefHandler.fromConfig(config)) else None

    val useExpansion: Boolean = config[Boolean]("useExpansion")
    val expansionHandler = if (useExpansion) Some(Expander.fromConfig(config[Config]("expander"))) else None

    new EidosActions(expansionHandler, corefHandler)
  }
}
