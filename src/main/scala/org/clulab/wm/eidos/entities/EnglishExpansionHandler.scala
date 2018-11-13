package org.clulab.wm.eidos.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.{DCTime, Property, Time}
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.utils.MentionUtils

import scala.collection.mutable.ArrayBuffer

class EnglishExpansionHandler extends TraversalBasedExpansion with LazyLogging {

  val MAX_HOPS_EXPANDING = 5
  val AVOID_LABEL = "Avoid-Strict"

  // avoid expanding along these dependencies
  override val INVALID_OUTGOING = Set[scala.util.matching.Regex](
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

  override val INVALID_INCOMING = Set[scala.util.matching.Regex](
    //"^nmod_with$".r,
    //    "^nmod_without$".r,
    //    "^nmod_except$".r
    //    "^nmod_despite$".r
  )

  // regexes describing valid outgoing dependencies
  override val VALID_OUTGOING = Set[scala.util.matching.Regex](
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

  override val VALID_INCOMING = Set[scala.util.matching.Regex](
    "^amod$".r,
    "^compound$".r,
    "^nmod_of".r
  )

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
        val expanded = expandIfNotAvoid(argToExpand, maxHops = MAX_HOPS_EXPANDING, stateToAvoid)
        expandedArgs.append(expanded)
        // Add the mention to the ones to avoid so we don't suck it up
        stateToAvoid = stateToAvoid.updated(Seq(expanded))
      }
      // Handle attachments
      val attached = expandedArgs
        .map(addSubsumedAttachments(_, state))
        .map(attachDCT(_, state))
        .map(addOverlappingAttachmentsTextBounds(_, state))
        .map(EntityHelper.trimEntityEdges(_, EntityHelper.INVALID_EDGE_TAGS))
      // Store
      newArgs.put(argType, attached)
    }
    // Return the event with the expanded args as well as the arg mentions themselves
    Seq(copyWithNewArgs(m, newArgs.toMap)) ++ newArgs.values.toSeq.flatten
  }



  /*
      Entity Expansion Helper Methods
   */

  def notInvalidConjunction(dep: String, hopsRemaining: Int): Boolean = {
    // If it's not a coordination/conjunction, don't worry
    if (EntityConstraints.COORD_DEPS.exists(pattern => pattern.findFirstIn(dep).isEmpty)) {
      return true
    } else if (hopsRemaining < MAX_HOPS_EXPANDING) {
      // if it has a coordination/conjunction, check to make sure not at the top level (i.e. we've traversed
      // something already
      return true
    }

    false
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
    if (dct.isDefined && ! m.attachments.exists(_.isInstanceOf[Time]))
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

  def apply() = new EnglishExpansionHandler()
}
