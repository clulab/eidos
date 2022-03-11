package org.clulab.wm.eidos.groundings.grounders.srl

import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Interval

import scala.collection.mutable.ArrayBuffer

// Helper class to hold a bunch of things that are needed throughout the process for grounding
case class SentenceHelper(sentence: Sentence, tokenInterval: Interval, exclude: Set[String]) {
  val chunks: Array[String] = sentence.chunks.get
  val chunkIntervals: Seq[Interval] = chunkSpans
  val words: Array[String] = sentence.words
  val srls: DirectedGraph[String] = sentence.enhancedSemanticRoles.get
  val dependencies: DirectedGraph[String] = sentence.dependencies.get
  val validTokenIndexes: IndexedSeq[Int] = tokenInterval.filterNot { index =>
    // The exluded words should have come from this very sentence, so it
    // should not be necessary to lowercase.  They should match exactly.
    exclude.contains(words(index))
  }
  // The roots of the SRL graph that are within the concept being grounded and aren't part of
  // an something we're ignoring (e.g., increase/decrease/quantification)
  val validPredicates: Seq[Int] = {
    // don't use the roots as here we're only interested in outgoing "sources" for predicates,
    // and the roots also contain the unreachable nodes etc.
    val original = srls.edges.map(edge => edge.source)
      // keep only predicates that are within the mention
      .filter(tokenInterval.contains)
      // remove the predicates which correspond to our increase/decrease/quantifiers
      .filter(validTokenIndexes contains _)
    // add back in ones that SRL "missed"
    val corrected = for {
      i <- tokenInterval
      if !original.contains(i)
      if outgoingOfType(i, Seq("compound")).nonEmpty
    } yield i
    // start with those closest to the syntactic root of the sentence to begin with "higher level" predicates
    (original ++ corrected).sortBy(minGraphDistanceToSyntacticRoot).distinct
  }

  // Find the shortest distance (in the syntax graph) between a given token and any of the roots
  def minGraphDistanceToSyntacticRoot(token: Int): Int = {
    // Get the sentence roots -- there can be more than one
    val roots = dependencies.roots
    if (roots.isEmpty) return 0
    // for each, check the shortest path from that root to the given token
    val pathLengths = roots.map(root => dependencies.shortestPath(root, token, ignoreDirection = true).length)
    // select the shortest to be the distance from the token to any of the roots
    pathLengths.min
  }

  val allTokensInvolvedInPredicates: Seq[Int] = {
    srls.allEdges
      .flatMap(edge => Seq(edge._1, edge._2))
//      .flatMap(tokenOrObjOfPreposition)
      .distinct
      .sorted
  }

  def findStart(chunkSpan: Interval, tok: Int): Int = {
    if (chunkSpan.start == tok) {
      tok
    } else {
      val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
      // not the first thing in the sequence
      val startLimit = if (currTokenIdx > 0) {
        allTokensInvolvedInPredicates(currTokenIdx - 1) + 1
      } else chunkSpan.start
      val start = math.max(chunkSpan.start, startLimit)
      start
    }
  }

  def findEnd(chunkSpan: Interval, tok: Int): Int = {
    if (chunkSpan.end == tok) {
      tok
    } else {
      val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
      // not the first thing in the sequence
      val endLimit = if (currTokenIdx < allTokensInvolvedInPredicates.length - 1) {
        allTokensInvolvedInPredicates(currTokenIdx + 1) // don't need to subtract 1 bc exclusive interval
      } else chunkSpan.end
      val end = math.min(chunkSpan.end, endLimit)
      end
    }
  }

  def chunkAvoidingSRLs(chunkSpan: Interval, tok: Int): Interval = {
    assert(allTokensInvolvedInPredicates.contains(tok))
    // val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)

    // If the tok starts the chunk
    val start = findStart(chunkSpan, tok)
    val end = findEnd(chunkSpan, tok)
    Interval(start, end)
  }

  // Make a Seq of the Intervals corresponding to the syntactic chunks in the sentence (inclusive, exclusive).
  def chunkSpans: Seq[Interval] = {
    val chunkIntervals = new ArrayBuffer[Interval]
    var currStart = -1
    var currEnd = -1
    val chunks = sentence.chunks.get
    val numTokens = chunks.length
    for ((t, i) <- chunks.zipWithIndex) {
      if (t.startsWith("B")) {
        // New chunk has started, package the previous
        if (currStart != -1 && currEnd != -1) {
          chunkIntervals.append(Interval(currStart, currEnd))
        }
        currStart = i
        currEnd = -1
      } else if (t.startsWith("I")) {
        // if this isn't the last thing in the sentence
        if (i + 1 < numTokens) {
          if (chunks(i + 1) != t) {
            // the next chunk is different, so this is the end
            currEnd = i + 1
          }
        }
      } else {
        // chunk starts with "O", if this is the first one, mark it as the end
        if (currEnd == -1) currEnd = i
      }
    }
    // Handle any remaining chunks that didn't get added yet
    if (currStart != -1 && currEnd != -1) {
      chunkIntervals.append(Interval(currStart, currEnd))
    }
    chunkIntervals
  }

  def wordsSlice(span: Interval): Array[String] = words.slice(span.start, span.end)
  def wordsSliceString(span: Interval): String = wordsSlice(span).mkString(" ")
  def tokenOrObjOfPreposition(tok: Int): Seq[Int] = {
    if (sentence.tags.get(tok) == "IN") {
      // if it's a preposition, follow the outgoing `case` dependency edge
      val out = dependencies
        // Get the next tokens that this token links to
        .incomingEdges(tok)
        // Get the subset that corresponds to the `case` dependency
        .collect{ case e if Seq("case", "mark").contains(e._2) => e._1 }
      out
    } else {
      Seq(tok)
    }
  }

  def outgoingOfType(tok: Int, constraints: Seq[String]): Array[Int] = {
    dependencies.outgoingEdges(tok)
      // keep only the ones that satisfy the constraints
      .filter(edge => constraints.contains(edge._2))
      // return the dsts
      .map(_._1)
  }

  def isArg(index: Int): Boolean = !isPred(index) &&
      srls.getIncomingEdges(index).nonEmpty && srls.getOutgoingEdges(index).isEmpty

  def isPred(index: Int): Boolean = validPredicates.contains(index)

  def isSkipword(index: Int): Boolean = !isKeepword(index)

  def isKeepword(index: Int): Boolean = {
    val word = sentence.words(index)
    val pos = sentence.tags.get(index)

    SentenceHelper.isKeepword(word, pos)
  }
}

object SentenceHelper {
  // TODO: Use TagSet for these.
  val skipwordPartsOfSpeech = Array(
    "CC",  // Coordinating conjunction
    "DT",  // Determiner
    "EX",  // Existential there
    "IN",  // Preposition or subordinating conjunction
    "POS", // Possessive ending
    "PRP", // Personal pronoun
    "TO",  // to
    ",",
    "."
  )
  // TODO: Use TagSet for these.
  val keepwordPartsOfSpeech = Array(
    "JJ", // Adjective
    "NN", // Noun, singular or mass
    "RB", // Adverb
    "VB"  // Verb, base form
  )

  def isSkipword(word: String, pos: String): Boolean = !isKeepword(word, pos)

  def isKeepword(word: String, pos: String): Boolean = keepwordPartsOfSpeech.exists(pos.startsWith)
}
