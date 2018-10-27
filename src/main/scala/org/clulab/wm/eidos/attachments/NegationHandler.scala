package org.clulab.wm.eidos.attachments

import org.clulab.odin._
import org.clulab.wm.eidos.EidosSystem
import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Interval


/**
  * Taken from Reach (1.4.0), the information system developed for the DARPA Big Mechanism program
  * https://github.com/clulab/reach
  *
  * Adapted for use in Eidos
  */
class NegationHandler(val language: String) {

  def detectNegations(mentions: Seq[Mention]): Seq[Mention] = {
    // do something very smart to handle negated events
    // and then return the mentions
    // Note that the approach can be different for different languages!
    language match {
      case "english" => mentions.map(detectNegationEnglish)
      case "portuguese" => ???
      case _ => throw new RuntimeException(s"Unsupported language: $language")
    }
  }

  def detectNegationEnglish(m: Mention): Mention = {
    m match {
      case event: EventMention =>
        // Dependency Negations
        val depNegations = gatherNegDepNegations(event)


        // Lexical Negations
        ///////////////////////////////////////////////////
        // Check for the presence of some negative verbs
        // in all the sentence except the tokens

        // First, extract the trigger's range from the mention
        val interval = event.trigger.tokenInterval

        // Gather left and right context
        val pairs = event.tokenInterval zip event.lemmas.get
        val pairsL = pairs takeWhile (_._1 < interval.start)
        val pairsR = pairs dropWhile (_._1 <= interval.end)

        // Get the evidence for the existing negations to avoid duplicates
        val evidence:Set[Int] = depNegations.flatMap(mod => mod.tokenInterval).toSet

        // Get the lexical negations
        val singleVerbNegations = gatherNegationVerbNegations(event, pairsL, pairsR, evidence)
        val bigramNegations = gatherBigramNegations(event, pairsL, pairsR, evidence)
        handleNegations(m.asInstanceOf[EventMention], (depNegations ++ singleVerbNegations ++ bigramNegations).toSet)

      case _ => m
    }
  }

  // Alter Negation modifications in-place
  def handleNegations(m: EventMention, negations: Set[Mention]): Mention = {
    if (m matches EidosSystem.RELATION_LABEL) {
      // count the negations
      val withNeg = negations match {
        // 0 Neg modifications means no change...
        case noChange if noChange.size == 0 => m
        // if we have an even number of Negations, remove them all
        case pos if pos.size % 2 == 0 => m
        // if we have an odd number, report only the first...
        case neg if neg.size % 2 != 0 =>
          val singleNeg =
            negations
              .toSeq
              .sortBy(_.tokenInterval)
              .head
          m.withAttachment(Negation(singleNeg.text, None))
      }
      withNeg
    } else m
  }

  def gatherNegDepNegations(event: EventMention):Seq[Mention] = {
    val dependencies = event.sentenceObj.dependencies

    /////////////////////////////////////////////////
    // Check the outgoing edges from the trigger looking
    // for a neg label
    val outgoing = dependencies match {
      case Some(deps) => deps.outgoingEdges
      case None => Array.empty
    }

    val negations = new ArrayBuffer[Mention]

    // Get the token interval of the event, but exclude the intervals of the arguments
    val argumentIntervals = event.arguments.values.flatten.map(_.tokenInterval)
    for {
      tok <- event.tokenInterval
      if !argumentIntervals.exists(_.contains(tok))
      out <- outgoing.lift(tok)
      (ix, label) <- out
      if label == "neg"
    } negations.append(
      new TextBoundMention(
        Seq("Negation_trigger"),
        Interval(ix),
        sentence = event.sentence,
        document = event.document,
        keep = event.keep,
        foundBy = event.foundBy
      )
    )

    negations
  }

  def gatherNegationVerbNegations(
                                   event: EventMention,
                                   leftContext: IndexedSeq[(Int, String)],
                                   rightContext: IndexedSeq[(Int, String)],
                                   previouslyFound: Set[Int]
                                 ): Seq[Mention] = {

    // Get the token interval of the event, but exclude the intervals of the arguments
    val argumentIntervals = event.arguments.values.flatten.map(_.tokenInterval)
    // Check for single-token negative verbs
    for {
      (ix, lemma) <- leftContext ++ rightContext
      if !argumentIntervals.exists(_.contains(ix))
      if (Seq("fail", "not") contains lemma) && !(previouslyFound contains ix)
    } yield new TextBoundMention(
      Seq("Negation_trigger"),
      Interval(ix),
      sentence = event.sentence,
      document = event.document,
      keep = event.keep,
      foundBy = event.foundBy
    )
  }

  def gatherBigramNegations(
                             event: EventMention,
                             leftContext: IndexedSeq[(Int, String)],
                             rightContext: IndexedSeq[(Int, String)],
                             previouslyFound: Set[Int]
                           ): Seq[Mention] = {

    def flattenTuples(left:(Int, String), right:(Int, String)) = {
      (
        (left._1, right._1),
        (left._2, right._2)
      )
    }

    // Get the token interval of the event, but exclude the intervals of the arguments
    val argumentIntervals = event.arguments.values.flatten.map(_.tokenInterval)

    val verbs = Seq(("play", "no"), ("play", "little"), ("is", "not"), ("be", "insufficient"))
    // Introduce bigrams for two-token verbs in both sides of the trigger
    for {
      side <- Seq(leftContext, rightContext)

      bigrams = (side zip side.slice(1, side.length)) map (x => flattenTuples(x._1, x._2))

      (interval, bigram) <- bigrams

      if !argumentIntervals.exists(_.contains(interval._1)) && !argumentIntervals.exists(_.contains(interval._2))

      if (verbs contains bigram) && (previouslyFound intersect (interval._1 to interval._2 + 1).toSet).isEmpty

    } yield new TextBoundMention(
      Seq("Negation_trigger"),
      Interval(interval._1, interval._2 + 1),
      sentence = event.sentence,
      document = event.document,
      keep = event.keep,
      foundBy = event.foundBy
    )
  }
}

object NegationHandler{

  def apply(language: String): NegationHandler = new NegationHandler(language)

}