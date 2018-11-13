package org.clulab.wm.eidos.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Sentence
import org.clulab.struct.Interval

import scala.annotation.tailrec

trait TraversalBasedExpansion extends ExpansionHandler with LazyLogging {

  val VALID_INCOMING   = Set[scala.util.matching.Regex]()

  val INVALID_INCOMING = Set[scala.util.matching.Regex]()

  // regexes describing valid outgoing dependencies
  val VALID_OUTGOING   = Set[scala.util.matching.Regex]()

  val INVALID_OUTGOING = Set[scala.util.matching.Regex]()


  /** Expand a mention along dependencies */
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    // Expand the incoming to recapture any triggers which were avoided
    val interval1 = traverseIncomingLocal(entity, maxHops, stateFromAvoid, entity.sentenceObj)
    val incomingExpanded = entity.asInstanceOf[TextBoundMention].copy(tokenInterval = interval1)
    // Expand on outgoing deps
    val interval2 = traverseOutgoingLocal(incomingExpanded, maxHops, stateFromAvoid, entity.sentenceObj)
    val outgoingExpanded = incomingExpanded.asInstanceOf[TextBoundMention].copy(tokenInterval = interval2)

    outgoingExpanded
  }

  // Do the expansion, but if the expansion causes you to suck up something we wanted to avoid, split at the
  // avoided thing and keep the half containing the original (pre-expansion) entity.
  def expandIfNotAvoid(orig: Mention, maxHops: Int, stateToAvoid: State): Mention = {
    val expanded = expand(orig, maxHops = maxHops, stateToAvoid)
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

  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  // NOTE: final so that we can access this method and override those it calls
  @tailrec
  final def traverseOutgoingLocal(
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
  def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State, sentence: Sentence): Interval = {
    val outgoing = outgoingEdges(m.sentenceObj)
    val incoming = incomingEdges(m.sentenceObj)
    traverseOutgoingLocal(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid, sentence)
  }

  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  // NOTE: final so that we can access this method and override those it calls
  @tailrec
  final def traverseIncomingLocal(
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
  def traverseIncomingLocal(m: Mention, numHops: Int, stateFromAvoid: State, sentence: Sentence): Interval = {
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
      VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
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
    VALID_INCOMING.exists(pattern => pattern.findFirstIn(dep).nonEmpty)
  }

    /** Ensure current token does not have any incoming dependencies that are invalid **/
  def hasValidIncomingDependencies(tokenIdx: Int, incoming: Array[Array[(Int, String)]]): Boolean = {
    if (incoming.nonEmpty && tokenIdx < incoming.length) {
      incoming(tokenIdx).forall(pair => !INVALID_INCOMING.exists(pattern => pattern.findFirstIn(pair._2).nonEmpty))
    } else true
  }

}
