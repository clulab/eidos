package org.clulab.wm.eidos.attachments

import org.clulab.odin._
import org.clulab.wm.eidos.EidosSystem
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
    val djskdjs = for (m <- mentions) {
      m match {
        case event: EventMention =>



          m
        // Resolve Negations and attach

        ///////////////////////////////////////////////////
        case _ => m
      }
    }


  }

  def detectNegation(m: Mention): Mention = {
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
      val negAttachments = negations match {
        // 0 or 1 Neg modifications means no change...
        case noChange if noChange.size <= 1 => Set.empty[Attachment]
        // if we have an even number of Negations, remove them all
        case pos if pos.size % 2 == 0 => Set.empty[Attachment]
        // if we have an odd number, report only the first...
        case neg if neg.size % 2 != 0 =>
          val singleNeg =
            negations
              .toSeq
              .sortBy(_.tokenInterval)
              .head
          Set()
      }

    }
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

    val negations: Seq[Mention] = for {
      tok <- event.tokenInterval
      out <- outgoing.lift(tok)
      (ix, label) <- out
      if label == "neg"
    } yield new TextBoundMention(
      Seq("Negation_trigger"),
      Interval(ix),
      sentence = event.sentence,
      document = event.document,
      keep = event.keep,
      foundBy = event.foundBy
    )

    negations
  }

  def gatherNegationVerbNegations(
                                   event: EventMention,
                                   leftContext: IndexedSeq[(Int, String)],
                                   rightContext: IndexedSeq[(Int, String)],
                                   previouslyFound: Set[Int]
                                 ): Seq[Mention] = {

    // Check for single-token negative verbs
    for {
      (ix, lemma) <- (leftContext ++ rightContext)
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

    val verbs = Seq(("play", "no"), ("play", "little"), ("is", "not"), ("be", "insufficient"))
    // Introduce bigrams for two-token verbs in both sides of the trigger
    for {
      side <- Seq(leftContext, rightContext)

      bigrams = (side zip side.slice(1, side.length)) map (x => flattenTuples(x._1, x._2))

      (interval, bigram) <- bigrams

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



  //  // Alter Negation modifications in-place
//  def handleNegations(ms: Seq[EidosMention]): Unit = {
//    ms foreach {
//      case m: EidosEventMention =>
//        val negationModifications = m.negations
//
//        // count the negations
//        negationModifications match {
//          // 0 or 1 Neg modifications means no change...
//          case noChange if noChange.size <= 1 => ()
//          // if we have an even number of Negations, remove them all
//          case pos if pos.size % 2 == 0 => m.negations = Set.empty
//          // if we have an odd number, report only the first...
//          case neg if neg.size % 2 != 0 =>
//            val singleNeg =
//              negationModifications
//                .toSeq
//                .sortBy(_.tokenInterval)
//                .head
//            m.negations = Set(singleNeg)
//        }
//      case _ => ()
//    }
//  }

}