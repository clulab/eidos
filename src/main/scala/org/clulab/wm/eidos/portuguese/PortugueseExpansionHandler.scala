package org.clulab.wm.eidos.portuguese

import org.clulab.wm.eidos.actions.{EnglishExpansionHandler, ExpansionHandler}
import org.clulab.processors.Sentence

// NOTE: Currently, we're using the same basic strategy for expansion as is used in the EnglishExpansionHandler
class PortugueseExpansionHandler extends EnglishExpansionHandler {

  // FIXME: Is this appropriate for PT?
  override val MAX_HOPS_EXPANDING = 5
  override val AVOID_LABEL = "Avoid-Strict"

  // FIXME: which are needed for PT?
  // avoid expanding along these dependencies
  override val INVALID_OUTGOING = Set[scala.util.matching.Regex](
    //    "^nmod_including$".r,
    //"acl:relcl".r,
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
    "^ref$".r,
    // portuguese
    "^nmod_já".r,
    "^nmod_exceto".r,
    "^nmod_dado".r,
    "^nmod_desde".r,
    "^nmod_dentre".r,
    "^nmod_sem$".r,
    "^appos".r,
    "^acl$".r,
  )

  // FIXME: which are needed for PT?
  override val INVALID_INCOMING = Set[scala.util.matching.Regex](
    //"^nmod_with$".r,
    //    "^nmod_without$".r,
    //    "^nmod_except$".r
    //    "^nmod_despite$".r
  )

  // FIXME: which are needed for PT?
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

  // FIXME: which are needed for PT?
  override val VALID_INCOMING = Set[scala.util.matching.Regex](
    "^amod$".r,
    "^compound$".r,
    "^nmod_of".r
  )

  /** Avoid expanding list-like stuff **/
  def commaCheck(sentence: Sentence, startIndex: Int, currentIndex: Int, dependency: String): Boolean = {
    // check to see if any intervening tokens are ",
    println(s"start: $startIndex")
    println(s"current: $currentIndex")
    println(s"dep: $dependency\n")
    val TO_CHECK = Set("amod", "xcomp")
    if (TO_CHECK.contains(dependency)) {
      // we should NOT have a preceding comma if the current dep is amod
      ! sentence.words.slice(startIndex, currentIndex).contains(",")
    } else true
  }

  /** Ensure dependency may be safely traversed */
  override def isValidOutgoingDependency(
    dep: String,
    startIndex: Int,
    currentIndex: Int,
    sentence: Sentence
  ): Boolean = {
    val token: String = sentence.words(currentIndex)

    (
      VALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        ! INVALID_OUTGOING.exists(pattern => pattern.findFirstIn(dep).nonEmpty) &&
        commaCheck(sentence, startIndex, currentIndex, dependency = dep)
      ) || (
      // Allow exception to close parens, etc.
      dep == "punct" && Seq(")", "]", "}", "-RRB-").contains(token)
      )
  }
}

object PortugueseExpansionHandler {
  def apply() = new PortugueseExpansionHandler()
}
