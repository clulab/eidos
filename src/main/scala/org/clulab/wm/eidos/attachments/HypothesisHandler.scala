package org.clulab.wm.eidos.attachments

import org.clulab.odin._
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.MentionUtils

/**
  * Taken from Reach (1.4.0), the information system developed for the DARPA Big Mechanism program
  * https://github.com/clulab/reach
  *
  * Adapted for use in Eidos
  */
class HypothesisHandler(hintsFile: String) {

  val degree = 2 // Degree up to which we should follow the links in the graph

  // These are the words that hint a hypothesis going on
  protected val hints = FileUtils.getCommentedTextSetFromResource(hintsFile)

  def detectHypotheses(mentions: Seq[Mention], state: State): Seq[Mention] = mentions.map(addAnyHedging)

  // Recursive function that helps us get the words outside the event
  def getSpannedIndexes(index: Int, degree: Int, dependencies: DirectedGraph[String]): Seq[Int] = {
    degree match {
      case 0 => Seq[Int]() // Base case of the recursion
      case _ =>

        val outgoing = dependencies.outgoingEdges
        val incoming = dependencies.incomingEdges

        // Get incoming and outgoing edges
        val t: Seq[(Int, String)] = incoming.lift(index) match {
          case Some(x) => x
          case None => Seq()
        }

        val edges = t ++ (outgoing.lift(index) match {
          case Some(x) => x
          case None => Seq()
        })

        // Each edge is a tuple of (endpoint index, edge label), so we map it to the first
        // element of the tuple
        val indexes: Seq[Int] = edges map (_._1)

        // Recursively call this function to get outter degrees
        val higherOrderIndexes: Seq[Int] = indexes flatMap (getSpannedIndexes(_, degree - 1, dependencies))

        indexes ++ higherOrderIndexes
    }
  }


  def addAnyHedging(mention: Mention): Mention = {
    // For now let's only look for hedging on Relations...
    if (mention matches EidosSystem.RELATION_LABEL) {

      // Get the dependencies of the sentence
      val dependencies = mention.sentenceObj.dependencies.getOrElse(new DirectedGraph[String](Nil, Set[Int]()))

      val eventInterval: Seq[Int] = mention.tokenInterval

      // Get the index of the word outside the event up to "degree" degrees
      val spannedIndexes: Seq[Int] = eventInterval flatMap (getSpannedIndexes(_, degree, dependencies))

      // Remove duplicates
      val indexes: Seq[Int] = (eventInterval ++ spannedIndexes).distinct

      // Get the lemmas
      val lemmas = indexes map (mention.sentenceObj.lemmas.get(_))

      // Get the hedging "hints"
      // Reach was making intervals for each, I don't think we need that here, but we can always
      // up the amount of structure we store.
      val hedgedLemmas = lemmas.filter(lemma => hints contains lemma)

      withApplicableHedging(mention, hedgedLemmas)
    } else {
      mention
    }
  }

  // If any hedging was found, store it as an attachment
  def withApplicableHedging(m: Mention, hedgedLemmas: Seq[String]): Mention = {
    val attachments = hedgedLemmas.map(Hedging(_, None))

    MentionUtils.withMoreAttachments(m, attachments)
  }
}

object HypothesisHandler {

  def apply(hintsFile: String) = new HypothesisHandler(hintsFile)
}