package org.clulab.wm.eidos.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention

import scala.annotation.tailrec
import scala.util.matching.Regex


/**
  * Utilities for validating entities
  */
object EntityConstraints extends LazyLogging {

  val VALID_FINAL_TAG = """^(DT|NN|VB|JJ|\-R[SR]B).*"""

  val COORD_DEPS: Set[Regex] = Set("^conj_".r, "^cc".r)

  // POS tags for splitting conjunctions

  val BRACKETS = Seq(
    ("(", ")"), ("-LRB-", "-RRB-"), // round
    ("{", "}"), ("-LCB-", "-RCB-"), // curly
    ("[", "]"), ("-LSB-", "-RSB-")  // square
  )

  // POS tags for splitting conjunctions
  val coordPOS = Set("CC", ",", "-LRB-", "-RRB-")

  // Ensure final token of mention span is valid
  def validFinalTag(mention: Mention): Boolean =
      mention.tags.isEmpty || mention.tags.get.last.matches(VALID_FINAL_TAG)

  // Limit entity mentions to at most n tokens
  def withinMaxLength(mention: Mention, n: Int): Boolean = mention.words.size <= n

  // Check if brackets and braces match
  def matchingBrackets(mention: Mention): Boolean =
      matchingBrackets(mention.words)

  def matchingBrackets(words: Seq[String]): Boolean =
      BRACKETS.forall(pair => matchingBrackets(words, pair._1, pair._2))

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
  def isCoord(i: Int, mention: Mention): Boolean = {
    def tag(n: Int) = mention.sentenceObj.tags.get(n)

    if (i > 0 && tag(i - 1).startsWith("JJ"))
      false
    else
      coordPOS.contains(tag(i))
  }
}
