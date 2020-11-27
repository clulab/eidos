package org.clulab.wm.eidos.expansion

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.extraction.EntityHelper
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.expansion.ArgumentExpander.logger
import org.clulab.wm.eidos.utils.TagSet

import scala.collection.mutable.ArrayBuffer

/**
  * Expander used to expand the arguments of Relation and EventMentions.
  * @param validArgs only expand arguments with these role labels
  * @param validLabels only expand arguments of mentions with these specified labels
  * @param dependencies the valid/invalid incoming/outgoing dependencies for expansion
  * @param maxHops max number of dependency edges will traverse during expansion
  */
class ArgumentExpander(validArgs: Set[String], validLabels: Set[String], dependencies: Dependencies, maxHops: Int, tagSet: TagSet) extends Expander {
  private val textBoundExpander = new TextBoundExpander(dependencies, maxHops)

  def expand(ms: Seq[Mention], state: State = new State()): Seq[Mention] = {
    val (expandable, other) = ms.partition(m => validLabels.contains(m.label))
    val expanded = expandArguments(expandable, state)
    expanded ++ other
  }

  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // Yields not only the mention with newly expanded arguments, but also yields the expanded argument mentions
    // themselves so that they can be added to the state (which happens when the Seq[Mentions] is returned at the
    // end of the action
    mentions.flatMap(expandArgs(_, state))
  }

  /**
    * Expand the arguments of a single mention.  If there is a trigger, the trigger limits the expansion.  The
    * arguments closest to the trigger are expanded first.
    * @param m mention with arguments to expand
    * @param state all mentions currently in state
    * @return mention with expanded args, as well as the expanded args themselves
    */
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

    // Get the trigger of the mention, if there is one so that it isn't expanded over
    val trigger = m match {
      case _: RelationMention => None
      case em: EventMention => Some(em.trigger)
      case _ => throw new RuntimeException("Trying to get the trigger from a mention with no trigger")
    }
    var stateToAvoid = if (trigger.nonEmpty) State(Seq(trigger.get)) else new State()

    // Make the new arguments
    val newArgs = scala.collection.mutable.HashMap[String, Seq[Mention]]()
    for ((argType, argMentions) <- m.arguments) {
      if (validArgs.contains(argType)) {
        // Sort, because we want to expand the closest first so they don't get subsumed
        val sortedClosestFirst = argMentions.sortBy(distToTrigger(trigger, _))
        val expandedArgs = new ArrayBuffer[Mention]
        // Expand each one, updating the state as we go
        for (argToExpand <- sortedClosestFirst) {
          val expanded = expandIfNotAvoid(argToExpand, maxHops, stateToAvoid)
          expandedArgs.append(expanded)
          // Add the mention to the ones to avoid so we don't suck it up
          stateToAvoid = stateToAvoid.updated(Seq(expanded))
        }

        // Handle attachments
        val attached = expandedArgs
          .map(ExpansionUtils.addSubsumedAttachments(_, state))
          .map(ExpansionUtils.attachDCT(_, state))
          .map(EntityHelper.trimEntityEdges(_, tagSet))
        // Store
        newArgs.put(argType, attached)
      } else {
        newArgs.put(argType, argMentions)
      }

    }
    // Return the event with the expanded args as well as the arg mentions themselves
    Seq(copyWithNewArgs(m, newArgs.toMap)) ++ newArgs.values.toSeq.flatten
  }

  // Do the expansion, but if the expansion causes you to suck up something we wanted to avoid, split at the
  // avoided thing and keep the half containing the original (pre-expansion) entity.
  def expandIfNotAvoid(orig: Mention, maxHops: Int, stateToAvoid: State): Mention = {
    val expanded = textBoundExpander.expand(orig, stateToAvoid)
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
        newMap = try {
          val origPath = argPathsMap(orig.arguments(argName).head)
          Map(expandedArgs(argName).head -> origPath)
        } catch {
          case _: java.util.NoSuchElementException => Map.empty[Mention, SynPath]
        }
      } yield (argName, newMap)

    // Make the copy based on the type of the Mention
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${orig.foundBy}_$foundByAffix" else orig.foundBy

    orig match {
      case _: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
      case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
      case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
    }
  }
}

object ArgumentExpander {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def fromConfig(config: Config, tagSet: TagSet): ArgumentExpander = {
    val validArgs: List[String] = config[List[String]]("validArguments")
    val validLabels: List[String] = config[List[String]]("validLabels")
    // Dependencies
    val validIncoming: List[String] = config[List[String]]("validIncoming")
    val invalidIncoming: List[String] = config[List[String]]("invalidIncoming")
    val validOutgoing: List[String] = config[List[String]]("validOutgoing")
    val invalidOutgoing: List[String] = config[List[String]]("invalidOutgoing")
    val expansionDeps = Dependencies(
      validIncoming.map(_.r).toSet,
      invalidIncoming.map(_.r).toSet,
      validOutgoing.map(_.r).toSet,
      invalidOutgoing.map(_.r).toSet
    )
    val maxHops: Int = config[Int]("maxHops")
    new ArgumentExpander(validArgs.toSet, validLabels.toSet, expansionDeps, maxHops, tagSet)
  }
}
