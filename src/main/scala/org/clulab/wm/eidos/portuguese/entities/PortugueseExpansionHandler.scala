package org.clulab.wm.eidos.portuguese.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.entities.TraversalBasedExpansion


class PortugueseExpansionHandler extends TraversalBasedExpansion with LazyLogging {

  // FIXME: which are needed for PT?
  // avoid expanding along these dependencies
  override val INVALID_OUTGOING: Set[scala.util.matching.Regex] = Set(
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
  override val INVALID_INCOMING: Set[scala.util.matching.Regex] = Set(
    //"^nmod_with$".r,
    //    "^nmod_without$".r,
    //    "^nmod_except$".r
    //    "^nmod_despite$".r
  )

  // FIXME: which are needed for PT?
  // regexes describing valid outgoing dependencies
  override val VALID_OUTGOING: Set[scala.util.matching.Regex] = Set(
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
  override val VALID_INCOMING: Set[scala.util.matching.Regex] = Set(
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
    // check to see if any intervening tokens are ","
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

  // FIXME: implement this
  /** Expands only entities */
  def expand(mentions: Seq[Mention], maxHops: Int, state: State): Seq[Mention] = {
    // Avoid **ALL** /Avoid.*/ mentions!
    val stateToAvoid: State = {
      val avoidMentions = state.allMentions.filter(_.matches("Avoid.*".r))
      State(avoidMentions)
    }

    // FIXME: is there a label/constant for this?
    val (entities, other) = mentions.partition(_.matches("Entity"))
    entities.map(expandIfNotAvoid(_, maxHops = maxHops, stateToAvoid))
  }

  override def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  /**
    * Ensure dependency may be safely traversed.  Inlcudes comma check.
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
      VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        noInterveningComma(sentence, sourceIndex, destIndex, dependency = dep, INVALID_OUTGOING_COMMA)
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
    VALID_INCOMING
      .exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
      noInterveningComma(sentence, sourceIndex, destIndex, dependency = dep,  INVALID_INCOMING_COMMA)
  }

}

object PortugueseExpansionHandler {

  val MAX_HOPS = 4

  def apply() = new PortugueseExpansionHandler()
}
