package org.clulab.wm.eidos.portuguese.entities

import org.clulab.odin._
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.actions.EnglishExpansionHandler
import org.clulab.wm.eidos.entities.EntityHelper

// NOTE: Currently, we're using the same basic strategy for expansion as is used in the EnglishExpansionHandler
class PortugueseExpansionHandler extends EnglishExpansionHandler {

  /**
    * Avoid expansions that introduce intervening commas when traversing certain of syntactic dependencies.
    * @param sentence An org.clulab.processors.Sentence
    * @param sourceIndex The token index from which the traversal begins
    * @param destIndex The token index to which the traversal leads
    * @param dependency A syntactic dependency (the relation's label)
    * @return Boolean indicating whether or not the expansion introduces an intervening comma
    */
  def noInterveningComma(
    sentence: Sentence,
    sourceIndex: Int,
    destIndex: Int,
    dependency: String,
    depToCheck: Set[String]
  ): Boolean = {
    // check to see if any intervening tokens are ",
    logger.debug(s"source:\t$sourceIndex")
    logger.debug(s"destination:\t$destIndex")
    logger.debug(s"dependency:\t$dependency")
    // TODO: decide whether or not to keep nsubj
    if (depToCheck.contains(dependency)) {
      // the traversal order may not correspond to the linear order of the tokens
      val trueStart = Seq(sourceIndex, destIndex).min
      val trueEnd = Seq(sourceIndex, destIndex).max
      // we should NOT have a preceding comma if the current dep is amod
      ! sentence.words.slice(trueStart, trueEnd).contains(",")
    } else true
  }

  // New action designed to expand the args of relevant events only...
  // FIXME: instead consider overriding EnglishExpansionHandler.expandArgs for our purposes
  override def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = {
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
      // Avoid **ALL** Avoid* mentions!
      stateToAvoid: State = {
        val res1 = state.allMentions.filter(_.matches("Avoid.*".r))
        val triggers: Seq[Mention] = if (trigger.nonEmpty) Seq(trigger.get) else Nil
        State(res1 ++ triggers)
      }

      // Get the argument map with the *expanded* Arguments
      expanded = for {
        (argType, argMentions) <- mention.arguments
        // Expand
        expandedMentions = argMentions.map(expandIfNotAvoid(_, maxHops = PortugueseExpansionHandler.MAX_HOPS_EXPANDING, stateToAvoid))
        // Handle the attachments for the newly expanded mention (make sure all previous and newly subsumed make it in!)
        attached         = expandedMentions.map(addSubsumedAttachments(_, state))
        dctattached      = attached.map(attachDCT(_, state))
        propAttached     = addOverlappingAttachmentsTextBounds(dctattached, state)
        // Trim the edges as needed
        trimmed          = propAttached.map(EntityHelper.trimEntityEdges)
      } yield (argType, trimmed)

    } yield Seq(copyWithNewArgs(mention, expanded.toMap)) ++ expanded.toSeq.unzip._2.flatten

    // Get all the new mentions for the state -- both the events with new args and the
    val res = expansionResult.flatten

    // Useful for debug
    res
  }

  // FIXME: can be removed if EnglishExpansionHandler.expandArgs is correctly overridden for our purposes
  def addOverlappingAttachmentsTextBounds(ms: Seq[Mention], state: State): Seq[Mention] = for {
    m <- ms
  } yield m match {
    case tb: TextBoundMention =>
      val attachments = getOverlappingAttachments(tb, state)
       if (attachments.nonEmpty) tb.copy(attachments = tb.attachments ++ attachments) else tb
    case _ => m
  }

  /**
    * Ensure dependency may be safely traversed
    * @param dep A syntactic dependency (the relation's label)
    * @param sourceIndex The token index from which the traversal begins
    * @param destIndex The token index to which the traversal leads
    * @param sentence An org.clulab.processors.Sentence
    * @return Boolean indicating whether or not the traversal is legal
    */
  override def isValidOutgoingDependency(
    dep: String,
    sourceIndex: Int,
    destIndex: Int,
    sentence: Sentence
  ): Boolean = {
    val token: String = sentence.words(destIndex)
    (
      PortugueseExpansionHandler.VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! PortugueseExpansionHandler.INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        noInterveningComma(sentence, sourceIndex, destIndex, dependency = dep, PortugueseExpansionHandler.INVALID_OUTGOING_COMMA)
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
  override def isValidIncomingDependency(
    dep: String,
    sourceIndex: Int,
    destIndex: Int,
    sentence: Sentence
  ): Boolean = {
    PortugueseExpansionHandler.VALID_INCOMING
      .exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
      noInterveningComma(sentence, sourceIndex, destIndex, dependency = dep,  PortugueseExpansionHandler.INVALID_INCOMING_COMMA)
  }

}

object PortugueseExpansionHandler {

  // FIXME: Is this appropriate for PT?
  val MAX_HOPS_EXPANDING = 4
  val AVOID_LABEL = "Avoid-Strict"

  // FIXME: which are needed for PT?
  // avoid expanding along these dependencies
  val INVALID_OUTGOING: Set[scala.util.matching.Regex] = Set(
    //    "^nmod_including$".r,
    "acl:relcl".r,
    "^case".r,
    "^conj".r,
    "^cc$".r,
    "^punct".r,
    "^ref$".r,
    // portuguese
    "^nmod_já".r,
    "^nmod_exceto".r,
    "^nmod_dado".r,
    "^nmod_desde".r,
    "^nmod_dentre".r,
    "^nmod_sem$".r,
    "^nmod_por$".r,
    "^nmod_com$".r,
    "cop".r
  )

  // FIXME: which are needed for PT?
  val INVALID_INCOMING: Set[scala.util.matching.Regex] = Set(
    //"^nmod_with$".r,
    //    "^nmod_without$".r,
    //    "^nmod_except$".r
    //    "^nmod_despite$".r
  )

  // FIXME: which are needed for PT?
  // regexes describing valid outgoing dependencies
  val VALID_OUTGOING: Set[scala.util.matching.Regex] = Set(
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

  // FIXME: which are needed for PT?
  val VALID_INCOMING: Set[scala.util.matching.Regex] = Set(
    "^amod$".r,
    "^compound$".r,
    "mark".r
    //"^nsubj:pass".r,
  )


  val INVALID_OUTGOING_COMMA: Set[String] = Set(
    "amod",
    "xcomp",
    "ccomp",
    "nsubj",
    "acl",
    "advcl",
    "advmod",
    "appos",
    "conj",
    "det",
    "obl"
  )

  val INVALID_INCOMING_COMMA: Set[String] = Set(
    "acl"
  )

  def apply() = new PortugueseExpansionHandler()
}
