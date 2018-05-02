package org.clulab.wm.eidos

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.struct.Interval
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.annotation.tailrec
import utils.DisplayUtils.displayMention
import EidosActions.{INVALID_INCOMING, INVALID_OUTGOING, VALID_OUTGOING}
import org.clulab.wm.eidos.entities.{EntityConstraints, EntityHelper}

import scala.collection.mutable.{Set => MutableSet}

// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping

class EidosActions(val taxonomy: Taxonomy) extends Actions with LazyLogging {

  val entityHelper = new EntityHelper
  val AVOID_LABEL = "Avoid"
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
        case _ => (0, 0, mention.attachments)
      }

      val argumentSize = attachmentsSet.toSeq.map(_.asInstanceOf[EidosAttachment].argumentSize).sum
      val triggerSize = mention.attachments.toSeq.map(_.asInstanceOf[TriggeredAttachment].trigger.length).sum
      val attachArgumentsSz = argumentSize + triggerSize

      (mention, (attachArgumentsSz + modSize + numArgs)) // The size of a mention is the sum of i) how many attachments are present ii) sum of args in each of the attachments iii) if (EventMention) ==>then include size of arguments
    }

    val maxModAttachSz = mention_attachmentSz.map(_._2).max
    val filteredMentions = mention_attachmentSz.filter(m => m._2 == maxModAttachSz).map(_._1)
    filteredMentions
  }

  def filterSubstringArgumentEvents(events: Seq[EventMention]): Seq[Mention] = {

    def argumentTexts(em: EventMention): Set[String] = em.arguments.values.toSet.flatten.map(m => m.text)

    // Return true if the event subsumes all the args
    def eventArgsSubsume(argsToCheck: Set[String], event: EventMention): Boolean = {
      val eventArgStrings = argumentTexts(event)
//      println("\t\t--> the existing event arguments I am checking: " + eventArgStrings.mkString(", "))
//      println("\t\t   --> the arguments I want to know if they are subsumed are: " + argsToCheck.mkString(", "))
//      println("\t\t      --> returning: " + subsumes(eventArgStrings, argsToCheck))
      subsumes(eventArgStrings, argsToCheck)
    }

    // True if A subsumes B
    def subsumes(A: Set[String], B: Set[String]): Boolean = B.forall(elem => contained(elem, A))
    def contained(s: String, A: Set[String]): Boolean = A.exists(elem => elem.contains(s))

    val triggerGroups = events.groupBy(event => (event.label, event.trigger))
    val filteredForArgumentSubsumption = for {
      (_, eventsInGroup) <- triggerGroups

      eventsKept = MutableSet[EventMention]() // Cache intermediate events.

      filtered = eventsInGroup
        // longest first
        .sortBy(- argTokenInterval(_).length)
        // Check to see if it's subsumed by something already there
        .filter { event =>
          val argTexts = argumentTexts(event)

          if (!eventsKept.exists(ev => eventArgsSubsume(argTexts, ev))) {
            eventsKept.add(event) // Add this event because it isn't subsumed by what's already there.
            //println(s"\t!!!!!!!!!!!!!!!\n\tEvent: ${event.text} is NOT subsumed -- adding to list!\n\t!!!!!!!!!!!!!!!")
            true // Keep the attachment.
          }
          else{
            //println(s"\t!!!!!!!!!!!!!!!\n\tEvent: ${event.text} IS subsumed -- filtering out!!\n\t!!!!!!!!!!!!!!!")
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


  // Remove incomplete Mentions
  def keepMostCompleteEvents(ms: Seq[Mention], state: State): Seq[Mention] = {

    val (baseEvents, nonEvents) = ms.partition(_.isInstanceOf[EventMention])
    // Filter out duplicate (or subsumed) events.  Strict containment used -- i.e. simply overlapping args is not
    // enough to be filtered out here.
    val events = filterSubstringArgumentEvents(baseEvents.map(_.asInstanceOf[EventMention]))

    // Entities
    val (textBounds, relationMentions) = nonEvents.partition(_.isInstanceOf[TextBoundMention])

    // remove incomplete entities (i.e. under specified when more fully specified exists)
    val tbMentionGroupings =
      textBounds.map(_.asInstanceOf[TextBoundMention]).groupBy(m => (m.tokenInterval, m.label, m.sentence))

    val completeTBMentions =
      for ((k, tbms) <- tbMentionGroupings) yield {
        //        val maxModSize: Int = tbms.map(tbm => tbm.attachments.size).max
        //        val filteredTBMs = tbms.filter(m => m.attachments.size == maxModSize)

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
    completeTBMentions.toSeq ++ relationMentions ++ completeEventMentions.toSeq
  }

  /*
      Mention State / Attachment Methods
   */

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
        val theme = tieBreaker(rm.arguments("theme")).asInstanceOf[TextBoundMention]
        theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = s"${theme.foundBy}++${rm.foundBy}")
      case em: EventMention =>
        val theme = tieBreaker(em.arguments("theme")).asInstanceOf[TextBoundMention]
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
      flattenedAttachments = entities.flatMap(_.attachments.map(_.asInstanceOf[TriggeredAttachment]))
      filteredAttachments = filterAttachments(flattenedAttachments)
    } yield {
      if (filteredAttachments.nonEmpty) {
        // use old version for now
//        val bestAttachment = filteredAttachments.sorted.reverse.head
//        val bestEntity = entities.find(_.attachments.find(_ eq bestAttachment) != None).get

        val bestAttachment = filteredAttachments.sorted.reverse.head
        // Since head was used above and there could have been a tie, == should be used below
        // The tie can be broken afterwards.
        val bestEntities = entities.filter(_.attachments.exists(_ == bestAttachment))
        val bestEntity = tieBreaker(bestEntities)

        copyWithAttachments(bestEntity, filteredAttachments)
      }
      else
        tieBreaker(entities)
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

  // New action designed to expand the args of relevant events only...
  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = {

    //mentions.map(m => displayMention(m))
    def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)
    def copyWithExpanded(orig: Mention, expandedArgs: Map[String, Seq[Mention]]): Mention = {

      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ expandedArgs.values.flatten.map(arg => arg.tokenInterval)
      //println("allIntervals: " + allIntervals.mkString(", "))
      // Find the largest span from these intervals
      val newTokenInterval = getNewTokenInterval(allIntervals)
      // Make the copy based on the type of the Mention
      orig match {
        case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
        case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval)
        case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval)
      }
    }

    val stateFromAvoid = State(state.allMentions.filter(_ matches AVOID_LABEL))

    //    val withExpansion = for {
//      mention <- mentions
//      expanded = for {
//        (argType, argMentions) <- mention.arguments
//        expandedMentions = argMentions.map(expand(_, maxHops = EidosActions.MAX_HOPS_EXPANDING, new State))
//        //splitMentions = expandedMentions.flatMap(entityHelper.splitCoordinatedEntities)
//      } yield (argType, expandedMentions)
//      (argNames, mentionsToCombine) = expanded.toSeq.unzip
//      cartesian = product(mentionsToCombine)
//      argMaps = recombine(argNames, cartesian)  // the new argument maps, each will correspond to a new mention
//
//    } yield argMaps.map(copyWithExpanded(mention, _))
//
//    withExpansion.flatten

    for {
      mention <- mentions
      expanded = for {
        (argType, argMentions) <- mention.arguments
        expandedMentions = argMentions.map(expandIfNotAvoid(_, maxHops = EidosActions.MAX_HOPS_EXPANDING, state))
        res = expandedMentions.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, AVOID_LABEL).isEmpty }
      } yield (argType, res)

    } yield copyWithExpanded(mention, expanded.toMap)

  }

  // Do the expansion, but if the expansion causes you to suck up something we want to avoid, keep the original instead
  // todo: perhaps we should handle this a diff way.... like, trim up tp the avoided thing?
  def expandIfNotAvoid(orig: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    val expanded = expand(orig, maxHops = EidosActions.MAX_HOPS_EXPANDING, stateFromAvoid)
    if (stateFromAvoid.mentionsFor(expanded.sentence, expanded.tokenInterval, AVOID_LABEL).isEmpty) {
      expanded
    } else {
      orig
    }

  }

  // Return the sequence of argMaps --> each will be a new Mention
  // mentionPairs: Seq[Seq[Mention]] -- each element of this is a ordered list of mentions such that the index of the
  //                                    mention corresponds to the argName in argNames
  def recombine(argNames: Seq[String], mentionPairs: Seq[Seq[Mention]]): Seq[Map[String, Seq[Mention]]] = {
    // Seq( ("cause", Seq(cause_mention)), ("effect", Seq(effect_mention)) ).toMap
    for {
      pair <- mentionPairs
      namedArgs = argNames.zip(pair).map(elem => (elem._1, Seq(elem._2)))
    } yield namedArgs.toMap
  }

  // cartesian product
  // from: List(List(x1, x2, x3), List(y1, y2))
  // to: List(List(x1, y1), List(x1, y2), List(x2, y1), List(x2, y2), List(x3, y1), List(x3, y2))
  private def product[A](xss: Seq[Seq[A]]) = xss.foldRight(Seq(Seq[A]())) {
    (xs, lla) => xs.flatMap(x => lla.map(x +: _))
  }

  //-- Entity expansion methods (brought in from EntityFinder)
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    val interval = traverseOutgoingLocal(entity, maxHops, stateFromAvoid)
    new TextBoundMention(entity.labels, interval, entity.sentence, entity.document, entity.keep, entity.foundBy)
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
                                     state: State
                                   ): Interval = {
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
      Interval(allTokens.min, allTokens.max + 1)
    } else {
      val newNewTokens = for{
        tok <- newTokens
        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
        (nextTok, dep) <- outgoingRelations(tok)
        if isValidOutgoingDependency(dep, remainingHops)
        if state.mentionsFor(sent, nextTok, AVOID_LABEL).isEmpty
        if hasValidIncomingDependencies(nextTok, incomingRelations)
      } yield nextTok
      traverseOutgoingLocal(tokens ++ newTokens, newNewTokens, outgoingRelations, incomingRelations, remainingHops - 1, sent, state)
    }
  }
  private def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State): Interval = {
    val outgoing = outgoingEdges(m.sentenceObj)
    val incoming = incomingEdges(m.sentenceObj)
    traverseOutgoingLocal(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid)
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
  def isValidOutgoingDependency(dep: String): Boolean = {
    VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
      ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
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

  def isValidOutgoingDependency(dep: String, hopsRemaining: Int): Boolean = {
    isValidOutgoingDependency(dep) //&& notInvalidConjunction(dep, hopsRemaining)
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

  // avoid expanding along these dependencies
  val INVALID_OUTGOING = Set[scala.util.matching.Regex](
//    "^nmod_including$".r,
//    "^nmod_without$".r,
//    "^nmod_except".r,
//    "^nmod_since".r,
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

  def apply(taxonomyPath: String) =
      new EidosActions(readTaxonomy(taxonomyPath))

  private def readTaxonomy(path: String): Taxonomy = {
    val input = FileUtils.getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }
}
