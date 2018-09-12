package org.clulab.wm.eidos

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.{Document, Sentence}
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils}
import org.clulab.struct.Interval
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.annotation.tailrec
import utils.DisplayUtils.{displayMention, shortDisplay}
import EidosActions.{INVALID_INCOMING, INVALID_OUTGOING, VALID_INCOMING, VALID_OUTGOING}
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.entities.{EntityConstraints, EntityHelper}

import scala.collection.mutable.{ArrayBuffer, Set => MutableSet}

import org.clulab.wm.eidos.document.TimeInterval

// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping

class EidosActions(val taxonomy: Taxonomy) extends Actions with LazyLogging {


  /*
      Global Action -- performed after each round in Odin
  */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // expand attachments

//    println("*************************************\n\t\tGLOBAL ACTION\n")
//    println("Incoming:")
//    mentions.foreach(displayMention)

    val (expandable, textBounds) = mentions.partition(m => EidosSystem.CAG_EDGES.contains(m.label))
    val expanded = expandArguments(expandable, state)
    val result = expanded ++ textBounds

    // Merge attachments
    val merged = mergeAttachments(result, state.updated(result))

    // Keep most complete
    val mostComplete = keepMostCompleteEvents(merged, state.updated(merged))

    // If the cause of an event is itself another event, replace it with the nested event's effect
    // collect all effects from causal events
    val (causal, nonCausal) = mostComplete.partition(m => EidosSystem.CAG_EDGES.contains(m.label))

    val assemble1 = createEventChain(causal, "effect", "cause")
    // FIXME please
    //val assemble2 = createEventChain(assemble1, "cause", "effect")
    val assemble2 = assemble1
    // FIXME
    // in the sentence below we stitch together the sequence of cause->effect events
    // but some expanded nounphrase remains, which shouldn't be displayed in the webapp
    // In Kenya , the shortened length of the main growing season , due in part to a delayed onset of seasonal rainfall , coupled with long dry spells and below-average rainfall is resulting in below-average production prospects in large parts of the eastern , central , and southern Rift Valley .
    val modifiedMentions = assemble2 ++ nonCausal

    val afterResolving = basicDeterminerCoref(modifiedMentions, state)

    // I know I'm an unnecessary line of code, but I am useful for debugging and there are a couple of things left to debug...
//    modifiedMentions ++ afterResolving

//    println("Outgoing:")
//    afterResolving.foreach(displayMention)
//    println("*************************************\n")

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
      Filtering Methods
   */

  // This was essentially .head before, but that is dependent on order.
  protected def tieBreaker(mentions: Seq[Mention]): Mention = {
    val oldBest = mentions.head
    val newBest = mentions.minBy(_.foundBy)

//    if (!oldBest.eq(newBest))
//      println("Changed answer")
    newBest
  }

  /**
    * @author Gus Hahn-Powell
    *         Copies the label of the lowest overlapping entity in the taxonomy
    */
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
      case _ => (0, 0, mention.attachments)
    }

    val argumentSize = attachmentsSet.toSeq.map(_.asInstanceOf[EidosAttachment].argumentSize).sum
    val triggerSize = mention.attachments.toSeq.filter(_.isInstanceOf[TriggeredAttachment]).map(_.asInstanceOf[TriggeredAttachment].trigger.length).sum
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
        .sortBy(ent => - (mentionAttachmentWeight(ent)))// + ent.tokenInterval.length))
        // Check to see if it's subsumed by something already there
        .filter { entity =>
          if (!entitiesKept.exists(ent => subsumesInterval(Set(ent.tokenInterval), Set(entity.tokenInterval)))) {
            entitiesKept.add(entity) // Add this event because it isn't subsumed by what's already there.
            true // Keep the attachment.
          }
          else{
            false
          }
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


    val triggerGroups = events.groupBy(event => (event.sentence, event.label, event.trigger))
    val filteredForArgumentSubsumption = for {
      (_, eventsInGroup) <- triggerGroups

      eventsKept = MutableSet[EventMention]() // Cache intermediate events.

      filtered = eventsInGroup
        // most args/longest/attachiest first
        .sortBy(event => - (mentionAttachmentWeight(event) + argTokenInterval(event).length))
        // Check to see if it's subsumed by something already there
        .filter { event =>
          val argTexts = argumentTexts(event)

          if (!eventsKept.exists(ev => eventArgsSubsume(argTexts, ev))) {
            eventsKept.add(event) // Add this event because it isn't subsumed by what's already there.
            true // Keep the attachment.
          }
          else{
            false
          }
        }
    } yield filtered

    filteredForArgumentSubsumption.toSeq.flatten
  }

  // We need to remove underspecified EventMentions of near-duplicate groupings
  // (ex. same phospho, but one is missing a site)
  def argTokenInterval(m: EventMention): Interval = {
    val min =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.start).toList.min
    val max =  m.arguments.values.toSeq.flatten.map(_.tokenInterval.end).toList.max
    Interval(start = min, end = max)
  }

  def importanceFromLengthAndAttachments(m: EventMention): Int = {
    val allArgMentions = m.arguments.values.toSeq.flatten.map(mention => mention.attachments.size)
    argTokenInterval(m).length
  }


  // Remove incomplete Mentions
  def keepMostCompleteEvents(ms: Seq[Mention], state: State): Seq[Mention] = {
//<<<<<<< HEAD

    val (baseEvents, nonEvents) = ms.partition(_.isInstanceOf[EventMention])
    // Filter out duplicate (or subsumed) events.  Strict containment used -- i.e. simply overlapping args is not
    // enough to be filtered out here.
    val events = filterSubstringArgumentEvents(baseEvents.map(_.asInstanceOf[EventMention]))

//=======
//    val (events, nonEvents) = ms.partition(_.isInstanceOf[EventMention])
//    val (textBounds, relationMentions) = nonEvents.partition(_.isInstanceOf[TextBoundMention])
//    // remove incomplete entities (i.e. under specified when more fully specified exists)
//>>>>>>> master

    // Entities
    val (baseTextBounds, relationMentions) = nonEvents.partition(_.isInstanceOf[TextBoundMention])
    val textBounds = filterSubstringEntities(baseTextBounds.map(_.asInstanceOf[TextBoundMention]))

    // remove incomplete entities (i.e. under specified when more fully specified exists)
    val tbMentionGroupings =
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, m.label, m.sentence))

    val completeTBMentions =
      for ((k, tbms) <- tbMentionGroupings) yield {

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
        val maxSize: Int = ems.map(_.arguments.values.flatten.size).max
        // max number of argument modifications
        // todo not all attachments are equal
        val filteredEMs = customAttachmentFilter(ems)
        tieBreaker(filteredEMs)
      }

    val res = completeTBMentions.toSeq ++ relationMentions ++ completeEventMentions.toSeq
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

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO Heather: write toy test for this
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
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
    for {
      m <- ms
      trigger = m.asInstanceOf[EventMention].trigger
      theme = tieBreaker(m.arguments("theme")).asInstanceOf[TextBoundMention]
      time: Option[TimeInterval] = m.document.asInstanceOf[EidosDocument].times(m.sentence).filter(_.span._1 == trigger.startOffset).headOption
    } yield time match {
      case None => theme
      case Some(t) => theme.withAttachment(new Time(t))
    }
  }

  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    println("DEBUG ACTION")
    ms
  }

  def getAttachment(mention: Mention): EidosAttachment = EidosAttachment.newEidosAttachment(mention)

  def addSubsumedAttachments(expanded: Mention, state: State): Mention = {
    def addAttachments(mention: Mention, attachments: Seq[Attachment], foundByName: String): Mention = {
      var out = mention
      for {
        a <- attachments
      } out = out.withAttachment(a)

      out match {
        case tb: TextBoundMention => tb.copy(foundBy=foundByName)
        case rm: RelationMention => rm.copy(foundBy=foundByName)
        case em: EventMention => em.copy(foundBy=foundByName)
      }
    }

    def compositionalFoundBy(ms: Seq[Mention]): String = {
      ms.map(_.foundBy).flatMap(ruleName => ruleName.split("\\+\\+")).distinct.mkString("++")
    }

    // find mentions of the same label and sentence overlap
    val overlapping = state.mentionsFor(expanded.sentence, expanded.tokenInterval)
//    println("Overlapping:")
//    overlapping.foreach(ov => println("  " + ov.text + ", " + ov.foundBy))
    val completeFoundBy = compositionalFoundBy(overlapping)

    val allAttachments = overlapping.flatMap(m => m.attachments).distinct
//    println(s"allAttachments: ${allAttachments.mkString(", ")}")
    // Add on all attachments
    addAttachments(expanded, allAttachments, completeFoundBy)
  }

  // Add the temporal attachments for any temporal expression
  def attachTemporal(mention: Mention, state: State): Mention = {
    val window = 10
    val timeIntervals = mention.document.asInstanceOf[EidosDocument].times(mention.sentence)
    var timeAttchment: Option[TimeInterval] = None
    for (interval <- timeIntervals)
      if (mention.startOffset - interval.span._2 <= window && interval.span._1 - mention.endOffset <= window)
        timeAttchment = Some(interval)

    timeAttchment match {
      case None => mention
      case Some(t) =>  mention.withAttachment(new Time(t))
    }
  }



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
        val bestEntity = tieBreaker(bestEntities)

        copyWithAttachments(bestEntity, filteredAttachments  ++ flattenedContextAttachments)
      }
      else
        tieBreaker(entities)
    }

    val res = keepMostCompleteEvents(mergedEntities.toSeq ++ nonentities, state)
    res
  }

  // Iteratively creates a mention which contains all of the passed in Attachments and no others
  def copyWithAttachments(mention: Mention, attachments: Seq[Attachment]): Mention = {
    // This is very inefficient, but the interface only allows for adding and subtracting one at a time.
    val attachmentless = mention.attachments.foldLeft(mention)((mention, attachment) => mention.withoutAttachment(attachment))

    attachments.foldLeft(attachmentless)((mention, attachment) => mention.withAttachment(attachment))
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

  /*
      Entity Expansion Methods
   */

  def pass(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)

  def copyWithNewArgs(orig: Mention, expandedArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    var newTokenInterval = orig.tokenInterval
    if (mkNewInterval) {
      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ expandedArgs.values.flatten.map(arg => arg.tokenInterval)
      // Find the largest span from these intervals
      newTokenInterval = getNewTokenInterval(allIntervals)
    }

      val paths = for {
        (argName, argPathsMap) <- orig.paths
        origPath = argPathsMap(orig.arguments(argName).head)
      } yield (argName, Map(expandedArgs(argName).head -> origPath))

    // Make the copy based on the type of the Mention
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${orig.foundBy}_$foundByAffix" else orig.foundBy

    orig match {
      case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
      case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
      case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
    }
  }


  // New action designed to expand the args of relevant events only...
  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = {

    //mentions.map(m => displayMention(m))


    // Yields not only the mention with newly expanded arguments, but also yields the expanded argument mentions
    // themselves so that they can be added to the state (which happens when the Seq[Mentions] is returned at the
    // end of the action
    val expansionResult = for {
      mention <- mentions
      trigger = mention match {
        case rm: RelationMention => None
        case em: EventMention => Some(em.trigger)
        case _ => throw new RuntimeException("Trying to get the trigger from a mention with no trigger")
      }
      stateToAvoid = if (trigger.nonEmpty) State(Seq(trigger.get)) else new State()

      // Get the argument map with the *expanded* Arguments
      expanded = for {
        (argType, argMentions) <- mention.arguments
        expandedMentions = argMentions.map(expandIfNotAvoid(_, maxHops = EidosActions.MAX_HOPS_EXPANDING, stateToAvoid))
        attached = expandedMentions.map(addSubsumedAttachments(_, state))
        //timeattached = attached.map(attachTemporal(_, state))
        //trimmed = timeattached.map(EntityHelper.trimEntityEdges)
        trimmed = attached.map(EntityHelper.trimEntityEdges)
      } yield (argType, trimmed)

    } yield Seq(copyWithNewArgs(mention, expanded.toMap)) ++ expanded.toSeq.unzip._2.flatten

    // Get all the new mentions for the state -- both the events with new args and the
    val res = expansionResult.flatten

    keepMostCompleteEvents(res, state.updated(res))
  }


  private def replaceMentionsInterval(m: Mention, i: Interval): Mention = m match {
    case m: TextBoundMention => m.copy(tokenInterval = i)
    case _ => sys.error("M is not a textboundmention, I don't know what to do")
  }

  // Do the expansion, but if the expansion causes you to suck up something we wanted to avoid, split at the
  // avoided thing and keep the half containing the origina (pre-expansion) entity.
  def expandIfNotAvoid(orig: Mention, maxHops: Int, stateToAvoid: State): Mention = {

    val expanded = expand(orig, maxHops = EidosActions.MAX_HOPS_EXPANDING, stateToAvoid)
    //println(s"orig: ${orig.text}\texpanded: ${expanded.text}")
   
    // split expanded at trigger (only thing in state to avoid)
    val triggerOption = stateToAvoid.mentionsFor(orig.sentence).headOption
    triggerOption match {
      case None => expanded
      case Some(trigger) =>
        // keep the half that is on the same side as original Mention
        if (trigger.tokenInterval overlaps expanded.tokenInterval) {
          if (orig.tokenInterval.end <= trigger.tokenInterval.start) {
            // orig is to the left of trigger
            replaceMentionsInterval(expanded, Interval(expanded.start, trigger.start))
          } else if (orig.tokenInterval.start >= trigger.tokenInterval.end) {
            // orig is to the right of trigger
            replaceMentionsInterval(expanded, Interval(trigger.end, expanded.end))
          } else {
            //throw new RuntimeException("original mention overlaps trigger")
            // This shouldn't happen, but Odin seems to handle this situation gracefully (by not extracting anything),
            // I guess here we'll do the same (i.e., not throw an exception)
            logger.debug(s"Unexpected overlap of trigger and argument: \n\t" +
              s"sent: [${orig.sentenceObj.getSentenceText}]\n\tRULE: " +
              s"${trigger.foundBy}\n\ttrigger: ${trigger.text}\torig: [${orig.text}]\n")
            orig
          }
        } else {
          expanded
        }
    }

  }


  //-- Entity expansion methods (brought in from EntityFinder)
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    val interval = traverseOutgoingLocal(entity, maxHops, stateFromAvoid, entity.sentenceObj)
    val outgoingExpanded = entity.asInstanceOf[TextBoundMention].copy(tokenInterval = interval)
    val interval2 = traverseIncomingLocal(outgoingExpanded, maxHops, stateFromAvoid, entity.sentenceObj)
    val incomingExpanded = outgoingExpanded.asInstanceOf[TextBoundMention].copy(tokenInterval = interval2)
    incomingExpanded
  }


  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  @tailrec
  private def traverseOutgoingLocal(
                                     tokens: Set[Int],
                                     newTokens: Set[Int],
                                     outgoingRelations: Array[Array[(Int, String)]],
                                     incomingRelations: Array[Array[(Int, String)]],
                                     remainingHops: Int,
                                     sent: Int,
                                     state: State,
                                     sentence: Sentence

                                   ): Interval = {
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
      Interval(allTokens.min, allTokens.max + 1)
    } else {
      val newNewTokens = for{
        tok <- newTokens
        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
        (nextTok, dep) <- outgoingRelations(tok)
        if isValidOutgoingDependency(dep, sentence.words(nextTok), remainingHops)
        if state.mentionsFor(sent, nextTok).isEmpty
        if hasValidIncomingDependencies(nextTok, incomingRelations)
      } yield nextTok
      traverseOutgoingLocal(tokens ++ newTokens, newNewTokens, outgoingRelations, incomingRelations, remainingHops - 1, sent, state, sentence)
    }
  }
  private def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State, sentence: Sentence): Interval = {
    val outgoing = outgoingEdges(m.sentenceObj)
    val incoming = incomingEdges(m.sentenceObj)
    traverseOutgoingLocal(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid, sentence)
  }

  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  @tailrec
  private def traverseIncomingLocal(
                                     tokens: Set[Int],
                                     newTokens: Set[Int],
                                     incomingRelations: Array[Array[(Int, String)]],
                                     remainingHops: Int,
                                     sent: Int,
                                     state: State,
                                     sentence: Sentence

                                   ): Interval = {
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
      Interval(allTokens.min, allTokens.max + 1)
    } else {
      val newNewTokens = for{
        tok <- newTokens
        if incomingRelations.nonEmpty && tok < incomingRelations.length
        (nextTok, dep) <- incomingRelations(tok)
        if isValidIncomingDependency(dep)
        if state.mentionsFor(sent, nextTok).isEmpty
      } yield nextTok
      traverseIncomingLocal(tokens ++ newTokens, newNewTokens, incomingRelations, remainingHops - 1, sent, state, sentence)
    }
  }
  private def traverseIncomingLocal(m: Mention, numHops: Int, stateFromAvoid: State, sentence: Sentence): Interval = {
    val incoming = incomingEdges(m.sentenceObj)
    traverseIncomingLocal(Set.empty, m.tokenInterval.toSet, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid, sentence)
  }



  def outgoingEdges(s: Sentence): Array[Array[(Int, String)]] = s.dependencies match {
    case None => sys.error("sentence has no dependencies")
    case Some(dependencies) => dependencies.outgoingEdges
  }

  def incomingEdges(s: Sentence): Array[Array[(Int, String)]] = s.dependencies match {
    case None => sys.error("sentence has no dependencies")
    case Some(dependencies) => dependencies.incomingEdges
  }

  /** Ensure dependency may be safely traversed */
  def isValidOutgoingDependency(dep: String, token: String, remainingHops: Int): Boolean = {
    (
      VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
      ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
      ) || (
        // Allow exception to close parens, etc.
        dep == "punct" && Seq(")", "]", "}", "-RRB-").contains(token)
      )
  }

  /** Ensure incoming dependency may be safely traversed */
  def isValidIncomingDependency(dep: String): Boolean = {
    VALID_INCOMING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
  }

  def notInvalidConjunction(dep: String, hopsRemaining: Int): Boolean = {
    // If it's not a coordination/conjunction, don't worry
    if (EntityConstraints.COORD_DEPS.exists(pattern => pattern.findFirstIn(dep).isEmpty)) {
      return true
    } else if (hopsRemaining < EidosActions.MAX_HOPS_EXPANDING) {
      // if it has a coordination/conjunction, check to make sure not at the top level (i.e. we've traversed
      // something already
      return true
    }

    false
  }

  /** Ensure current token does not have any incoming dependencies that are invalid **/
  def hasValidIncomingDependencies(tokenIdx: Int, incomingDependencies: Array[Array[(Int, String)]]): Boolean = {
    if (incomingDependencies.nonEmpty && tokenIdx < incomingDependencies.length) {
      incomingDependencies(tokenIdx).forall(pair => ! INVALID_INCOMING.exists(pattern => pattern.findFirstIn(pair._2).nonEmpty))
    } else true
  }



}

object EidosActions extends Actions {

  val MAX_HOPS_EXPANDING = 5
  val AVOID_LABEL = "Avoid-Strict"

  // Used for simplistic coreference identification
  val COREF_DETERMINERS: Set[String] = Set("this", "that", "these", "those")
  val ANTECEDENT: String = "antecedent"
  val ANAPHOR: String = "anaphor"

  
  // avoid expanding along these dependencies
  val INVALID_OUTGOING = Set[scala.util.matching.Regex](
//    "^nmod_including$".r,
    "^nmod_without$".r,
    "^nmod_except".r,
    "^nmod_since".r,
    "^nmod_as".r,
    "^nmod_due_to".r,
//    "^nmod_among".r
    "^conj".r,
    "^cc".r,
    "^punct".r

  )

  val INVALID_INCOMING = Set[scala.util.matching.Regex](
    //"^nmod_with$".r,
//    "^nmod_without$".r,
//    "^nmod_except$".r
//    "^nmod_despite$".r
  )

  // regexes describing valid outgoing dependencies
  val VALID_OUTGOING = Set[scala.util.matching.Regex](
//    "^amod$".r, "^advmod$".r,
//    "^dobj$".r,
//    "^compound".r, // replaces nn
//    "^name".r, // this is equivalent to compound when NPs are tagged as named entities, otherwise unpopulated
//    // ex.  "isotonic fluids may reduce the risk" -> "isotonic fluids may reduce the risk associated with X.
//    "^acl_to".r, // replaces vmod
//    "xcomp".r, // replaces vmod
//    // Changed from processors......
//    "^nmod".r, // replaces prep_
//    //    "case".r
//    "^ccomp".r
    ".+".r
  )

  val VALID_INCOMING = Set[scala.util.matching.Regex](
    "^amod$".r,
    "^compound$".r
  )

  def apply(taxonomyPath: String) =
      new EidosActions(readTaxonomy(taxonomyPath))

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
