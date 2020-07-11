package org.clulab.wm.eidos.extraction

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.TagSet

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * Utilities for validating entities
  */
object EntityConstraints extends LazyLogging {

  val COORD_DEPS: Set[Regex] = Set("^conj_".r, "^cc".r)

  // Ensure final token of mention span is valid
  def validFinalTag(mention: Mention, tagSet: TagSet): Boolean =
    mention.tags.isEmpty || tagSet.isValidFinal(mention.tags.get.last)

  // Limit entity mentions to at most n tokens
  def withinMaxLength(mention: Mention, n: Int): Boolean = mention.words.size <= n

  // Check if brackets and braces match
  def matchingBrackets(mention: Mention): Boolean =
    matchingBrackets(mention.words)

  def matchingBrackets(words: Seq[String]): Boolean =
    TagSet.BRACKETS.forall(pair => matchingBrackets(words, pair._1, pair._2))

  // Each of the brackets is on a different "channel" so that ([)] is valid.
  // Otherwise, a stack of outstanding unmatched brackets is required.
  def matchingBrackets(words: Seq[String], opening: String, closing: String): Boolean = {


    @tailrec
    def matchingBrackets(index: Int, extraOpening: Int): Boolean = {
      if (extraOpening < 0)
        false // too many closing without opening
      else if (index >= words.length)
        extraOpening == 0 // if it is just right
      else if (words(index) == opening)
        matchingBrackets(index + 1, extraOpening + 1)
      else if (words(index) == closing)
        matchingBrackets(index + 1, extraOpening - 1)
      else
        matchingBrackets(index + 1, extraOpening)
    }


    matchingBrackets(0, 0)
  }

  // Decide if the sentence element is a conjunction using just the POS tag
  def isConjunction(i: Int, mention: Mention, tagSet: TagSet): Boolean =
      if (i > 0 && tagSet.isAnyAdjective(mention.sentenceObj.tags.get(i - 1))) false
      else tagSet.isCoordinating(mention.sentenceObj.tags.get(i))
}
