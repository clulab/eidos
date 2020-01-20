package org.clulab.wm.eidos.expansion

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.extraction.EntityConstraints
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.expansion.TextBoundExpander.logger

import scala.annotation.tailrec

/**
  * Expands mentions along specified incoming and outgoing syntactic dependencies.
  * @param dependencies  Specifies the valid/invalid incoming/outgoing dependencies
  * @param maxHops Max number of dependency edges will traverse during expansion
  */
class TextBoundExpander(dependencies: Dependencies, maxHops: Int) extends Expander {

  def expand(ms: Seq[Mention], avoidState: State = new State()): Seq[Mention] = {
    ms.map(expand(_, avoidState))
  }

  /**
    * Based on the Expansion from clulab processors.  Expand a mention along syntactic dependencies, as long
    * as you don't cross into something you want to avoid.  Mentions are returned as TextBoundMentions
    * @param entity Mention to expand
    * @param stateFromAvoid mentions representing items which you don't want to expand into
    * @return expanded TextBoundMention
    */
  def expand(entity: Mention, stateFromAvoid: State): Mention = {
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

  private def crossesTrigger(sent: Int, orig: Interval, nextTok: Int, triggerState: State): Boolean = {
    val trigger = triggerState.mentionsFor(sent).headOption
    // if there is no trigger, it can't be crossed
    if (trigger.isEmpty) {
      return false
    }
    // If you're landing on the trigger, it crosses!
    if (triggerState.mentionsFor(sent, nextTok).nonEmpty) {
      return true
    }
    val triggerInterval = trigger.get.tokenInterval
    // orig to the left of the trigger
    if (orig.end < triggerInterval.start) {
      nextTok > triggerInterval.start
    } else if (orig.start > triggerInterval.end) {
      nextTok < triggerInterval.end
    } else {
//      logger.warn("Trigger and original entity overlap")
      true
    }
    // orig to the right of the trigger
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
        currMentionTokens = tokens ++ newTokens
        if !crossesTrigger(sent, Interval(currMentionTokens.min, currMentionTokens.max + 1), nextTok, state)
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
      dependencies.validOutgoing.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! dependencies.invalidOutgoing.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
      ) || (
      // Allow exception to close parens, etc.
      dep == "punct" && Seq(")", "]", "}", "-RRB-").contains(token)
      )
  }

  /** Ensure incoming dependency may be safely traversed */
  def isValidIncomingDependency(dep: String): Boolean = {
    dependencies.validIncoming.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
  }

  def notInvalidConjunction(dep: String, hopsRemaining: Int): Boolean = {
    // If it's not a coordination/conjunction, don't worry
    if (EntityConstraints.COORD_DEPS.exists(pattern => pattern.findFirstIn(dep).isEmpty)) {
      return true
    } else if (hopsRemaining < maxHops) {
      // if it has a coordination/conjunction, check to make sure not at the top level (i.e. we've traversed
      // something already
      return true
    }

    false
  }

  /** Ensure current token does not have any incoming dependencies that are invalid **/
  def hasValidIncomingDependencies(tokenIdx: Int, incomingDependencies: Array[Array[(Int, String)]]): Boolean = {
    if (incomingDependencies.nonEmpty && tokenIdx < incomingDependencies.length) {
      incomingDependencies(tokenIdx).forall(pair => ! dependencies.invalidIncoming.exists(pattern => pattern.findFirstIn(pair._2).nonEmpty))
    } else true
  }
}

object TextBoundExpander {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def fromConfig(config: Config): TextBoundExpander = {
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
    new TextBoundExpander(expansionDeps, maxHops)
  }
}