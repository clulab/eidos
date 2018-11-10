package org.clulab.wm.eidos.actions

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.{DCTime, Property, Time}
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.entities.{EntityConstraints, EntityHelper}
import org.clulab.wm.eidos.utils.MentionUtils

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class EnglishExpansionHandler extends ExpansionHandler with LazyLogging {

  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // Yields not only the mention with newly expanded arguments, but also yields the expanded argument mentions
    // themselves so that they can be added to the state (which happens when the Seq[Mentions] is returned at the
    // end of the action
    val res = mentions.flatMap(expandArgs(_, state))

    // Useful for debug
    res
  }

  def expandArgs(m: Mention, state: State): Seq[Mention] = {
    // Helper method to figure out which mentions are the closest to the trigger
    def distToTrigger(trigger: Option[TextBoundMention], m: Mention): Int = {
      if (trigger.isDefined) {
        // get the trigger
        val t = trigger.get
        if (m.start < t.start) {
          // mention to the left of the trigger
          math.abs(t.start - m.end)
        } else if (m.start > t.end) {
          // mention to the right of the trigger
          math.abs(m.start - t.end)
        } else {
          logger.debug(s"Unexpected overlap of trigger and argument: \n\t" +
            s"sent: [${m.sentenceObj.getSentenceText}]\n\tRULE: " +
            s"${t.foundBy}\n\ttrigger: ${t.text}\torig: [${m.text}]\n")
          m.start
        }
      } else {
        m.start
      }
    }
    // Get the trigger of the mention, if there is one
    val trigger = m match {
      case rm: RelationMention => None
      case em: EventMention => Some(em.trigger)
      case _ => throw new RuntimeException("Trying to get the trigger from a mention with no trigger")
    }
    var stateToAvoid = if (trigger.nonEmpty) State(Seq(trigger.get)) else new State()

    // Make the new arguments
    val newArgs = scala.collection.mutable.HashMap[String, Seq[Mention]]()
    for ((argType, argMentions) <- m.arguments) {
      // Sort, because we want to expand the closest first so they don't get subsumed
      val sortedClosestFirst = argMentions.sortBy(distToTrigger(trigger, _))
      val expandedArgs = new ArrayBuffer[Mention]
      // Expand each one, updating the state as we go
      for (argToExpand <- sortedClosestFirst) {
        val expanded = expandIfNotAvoid(argToExpand, maxHops = EnglishExpansionHandler.MAX_HOPS_EXPANDING, stateToAvoid)
        expandedArgs.append(expanded)
        // Add the mention to the ones to avoid so we don't suck it up
        stateToAvoid = stateToAvoid.updated(Seq(expanded))
      }
      // Handle attachments
      val attached = expandedArgs
        .map(addSubsumedAttachments(_, state))
        .map(attachDCT(_, state))
        .map(addOverlappingAttachmentsTextBounds(_, state))
        .map(EntityHelper.trimEntityEdges)
      // Store
      newArgs.put(argType, attached)
    }
    // Return the event with the expanded args as well as the arg mentions themselves
    Seq(copyWithNewArgs(m, newArgs.toMap)) ++ newArgs.values.toSeq.flatten
  }



  /*
      Entity Expansion Helper Methods
   */

  // Do the expansion, but if the expansion causes you to suck up something we wanted to avoid, split at the
  // avoided thing and keep the half containing the original (pre-expansion) entity.
  def expandIfNotAvoid(orig: Mention, maxHops: Int, stateToAvoid: State): Mention = {
    val expanded = expand(orig, maxHops = EnglishExpansionHandler.MAX_HOPS_EXPANDING, stateToAvoid)
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

  private def replaceMentionsInterval(m: Mention, i: Interval): Mention = m match {
    case m: TextBoundMention => m.copy(tokenInterval = i)
    case _ => sys.error("M is not a textboundmention, I don't know what to do")
  }

  //-- Entity expansion methods (brought in from EntityFinder)
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    // Expand the incoming to recapture any triggers which were avoided
    val interval1 = traverseIncomingLocal(entity, maxHops, stateFromAvoid, entity.sentenceObj)
    val incomingExpanded = entity.asInstanceOf[TextBoundMention].copy(tokenInterval = interval1)
    // Expand on outgoing deps
    val interval2 = traverseOutgoingLocal(incomingExpanded, maxHops, stateFromAvoid, entity.sentenceObj)
    val outgoingExpanded = incomingExpanded.asInstanceOf[TextBoundMention].copy(tokenInterval = interval2)

    outgoingExpanded
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
      val sourceIdx = (tokens ++ newTokens).min // earliest in current set
      val newNewTokens = for{
        tok <- newTokens
        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
        (nextTok, dep) <- outgoingRelations(tok)
        if isValidOutgoingDependency(dep = dep, sourceIndex = sourceIdx, destIndex = nextTok, sentence = sentence)
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
      val sourceIdx = (tokens ++ newTokens).min // earliest in current set
      val newNewTokens = for{
        tok <- newTokens
        if incomingRelations.nonEmpty && tok < incomingRelations.length
        (nextTok, dep) <- incomingRelations(tok)
        if isValidIncomingDependency(dep = dep, sourceIndex = sourceIdx, destIndex = nextTok, sentence = sentence)
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

  def isValidOutgoingDependency(
    dep: String,
    sourceIndex: Int,
    destIndex: Int,
    sentence: Sentence
  ): Boolean = {
    val token: String = sentence.words(destIndex)

    (
      EnglishExpansionHandler.VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! EnglishExpansionHandler.INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
      ) || (
      // Allow exception to close parens, etc.
      dep == "punct" && Seq(")", "]", "}", "-RRB-").contains(token)
      )
  }
  /**
    * Ensure incoming dependency may be safely traversed
    * @param dep A syntactic dependency (the relation's label)
    * @param sourceIndex The token index from which the traversal begins
    * @param destIndex The token index to which the traversal leads
    * @param sentence An org.clulab.processors.Sentence
    * @return Boolean indicating whether or not the traversal is legal
    */
  def isValidIncomingDependency(
    dep: String,
    sourceIndex: Int,
    destIndex: Int,
    sentence: Sentence
  ): Boolean = {
    EnglishExpansionHandler.VALID_INCOMING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
  }

  def notInvalidConjunction(dep: String, hopsRemaining: Int): Boolean = {
    // If it's not a coordination/conjunction, don't worry
    if (EntityConstraints.COORD_DEPS.exists(pattern => pattern.findFirstIn(dep).isEmpty)) {
      return true
    } else if (hopsRemaining < EnglishExpansionHandler.MAX_HOPS_EXPANDING) {
      // if it has a coordination/conjunction, check to make sure not at the top level (i.e. we've traversed
      // something already
      return true
    }

    false
  }

  /** Ensure current token does not have any incoming dependencies that are invalid **/
  def hasValidIncomingDependencies(tokenIdx: Int, incomingDependencies: Array[Array[(Int, String)]]): Boolean = {
    if (incomingDependencies.nonEmpty && tokenIdx < incomingDependencies.length) {
      incomingDependencies(tokenIdx).forall(pair => ! EnglishExpansionHandler.INVALID_INCOMING.exists(pattern => pattern.findFirstIn(pair._2).nonEmpty))
    } else true
  }

  // Return a copy of the orig EventMention, but with the expanded arguments
  // The changes made to the mention are the args, the token interval, foundby, and the paths.
  def copyWithNewArgs(orig: Mention, expandedArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    // Helper method to get a token interval for the new event mention with expanded args
    def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)

    val newTokenInterval = if (mkNewInterval) {
      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ expandedArgs.values.flatten.map(arg => arg.tokenInterval)
      // Find the largest span from these intervals
      getNewTokenInterval(allIntervals)
    }
    else orig.tokenInterval

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


  /*
      Attachments helper methods
   */

  // During expansion, sometimes there are attachments that got sucked up, here we add them to the expanded argument mention
  def addSubsumedAttachments(expanded: Mention, state: State): Mention = {
    def addAttachments(mention: Mention, attachments: Seq[Attachment], foundByName: String): Mention = {
      val out = MentionUtils.withMoreAttachments(mention, attachments)

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

  // Add the document creation time (dct) attachment if there is no temporal attachment
  // i.e., a backoff
  def attachDCT(m: Mention, state: State): Mention = {
    val dct = m.document.asInstanceOf[EidosDocument].getDCT()
    if (dct.isDefined && m.attachments.filter(_.isInstanceOf[Time]).isEmpty)
      m.withAttachment(DCTime(dct.get))
    else
      m
  }

  def addOverlappingAttachmentsTextBounds(m: Mention, state: State): Mention = {
    m match {
      case tb: TextBoundMention =>
        val attachments = getOverlappingAttachments(tb, state)
        if (attachments.nonEmpty) tb.copy(attachments = tb.attachments ++ attachments) else tb
      case _ => m
    }
  }


  def getOverlappingAttachments(m: Mention, state: State): Set[Attachment] = {
    val interval = m.tokenInterval
    // TODO: Currently this is only Property attachments, but we can do more too
    val overlappingProps = state.mentionsFor(m.sentence, interval, label = "Property")
    overlappingProps.map(pm => Property(pm.text, None)).toSet
  }


}

object EnglishExpansionHandler {

  val MAX_HOPS_EXPANDING = 5
  val AVOID_LABEL = "Avoid-Strict"

  // avoid expanding along these dependencies
  val INVALID_OUTGOING = Set[scala.util.matching.Regex](
    //    "^nmod_including$".r,
    "acl:relcl".r,
    "advcl_to".r,
    "^advcl_because".r,
    "^case".r,
    "^conj".r,
    "^cc$".r,
    "^nmod_as".r,
    "^nmod_because".r,
    "^nmod_due_to".r,
    "^nmod_except".r,
    "^nmod_given".r,
    "^nmod_since".r,
    "^nmod_without$".r,
    "^punct".r,
    "^ref$".r
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
    "^compound$".r,
    "^nmod_of".r
  )

  def apply() = new EnglishExpansionHandler()
}
