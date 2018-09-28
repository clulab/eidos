package org.clulab.wm.eidos.attachments

import org.clulab.odin._
//import org.clulab.reach.mentions._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}


/**
  * Taken from Reach (1.4.0), the information system developed for the DARPA Big Mechanism program
  * https://github.com/clulab/reach
  *
  * Adapted for use in Eidos
  */
object NegationHandler {

  def detectNegations(mentions: Seq[EidosMention]): Seq[EidosMention] = {
    val state = State(mentions.map(_.odinMention))
    // do something very smart to handle negated events
    // and then return the mentions

    // Iterate over the BioEventMentions
    mentions foreach {
      case event: EidosEventMention =>

        val dependencies = event.odinMention.sentenceObj.dependencies

        /////////////////////////////////////////////////
        // Check the outgoing edges from the trigger looking
        // for a neg label
        val outgoing = dependencies match {
          case Some(deps) => deps.outgoingEdges
          case None => Array.empty
        }

        for{
          tok <- event.odinMention.tokenInterval
          out <- outgoing.lift(tok)
          (ix, label) <- out
          if label == "neg"
        }
          event.negations += new TextBoundMention(
            Seq("Negation_trigger"),
            Interval(ix),
            sentence = event.odinMention.sentence,
            document = event.odinMention.document,
            keep = event.odinMention.keep,
            foundBy = event.odinMention.foundBy
          )
        ///////////////////////////////////////////////////

        ///////////////////////////////////////////////////
        // Check for the presence of some negative verbs
        // in all the sentence except the tokens

        // First, extract the trigger's range from the mention
        val interval = event.odinTrigger.tokenInterval

        //val pairs = for (lemma <- event.lemmas) yield (1, lemma)
        val pairs = event.odinMention.tokenInterval zip event.odinMention.lemmas.get

        val pairsL = pairs takeWhile (_._1 < interval.start)
        val pairsR = pairs dropWhile (_._1 <= interval.end)

        // Get the evidence for the existing negations to avoid duplicates
        val evidence:Set[Int] = event.negations.flatMap(mod => mod.tokenInterval)

        // Check for single-token negative verbs
        for{
          (ix, lemma) <- (pairsL ++ pairsR)
          if (Seq("fail", "not") contains lemma) && !(evidence contains ix)
        }{
          event.negations += new TextBoundMention(
            Seq("Negation_trigger"),
            Interval(ix),
            sentence = event.odinMention.sentence,
            document = event.odinMention.document,
            keep = event.odinMention.keep,
            foundBy = event.odinMention.foundBy
          )
        }

        def flattenTuples(left:(Int, String), right:(Int, String)) = {
          (
            (left._1, right._1),
            (left._2, right._2)
          )
        }

        val verbs = Seq(("play", "no"), ("play", "little"), ("is", "not"), ("be", "insufficient"))
        // Introduce bigrams for two-token verbs in both sides of the trigger
        for(side <- Seq(pairsL, pairsR)){
          val bigrams = (side zip side.slice(1, side.length)) map (x =>
            flattenTuples(x._1, x._2)
            )

          for{
            (interval, bigram) <- bigrams
            if (verbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
          }
          {
            event.negations += new TextBoundMention(
              Seq("Negation_trigger"),
              Interval(interval._1, interval._2 + 1),
              sentence = event.odinMention.sentence,
              document = event.odinMention.document,
              keep = event.odinMention.keep,
              foundBy = event.odinMention.foundBy
            )
          }
        }
      ///////////////////////////////////////////////////
      case _ => ()
    }

    mentions
  }

  // Alter Negation modifications in-place
  def handleNegations(ms: Seq[EidosMention]): Unit = {
    ms foreach {
      case m: EidosEventMention =>
        val negationModifications = m.negations

        // count the negations
        negationModifications match {
          // 0 or 1 Neg modifications means no change...
          case noChange if noChange.size <= 1 => ()
          // if we have an even number of Negations, remove them all
          case pos if pos.size % 2 == 0 => m.negations = Set.empty
          // if we have an odd number, report only the first...
          case neg if neg.size % 2 != 0 =>
            val singleNeg =
              negationModifications
                .toSeq
                .sortBy(_.tokenInterval)
                .head
            m.negations = Set(singleNeg)
        }
      case _ => ()
    }
  }

}