package org.clulab.wm.eidos.serialization.web

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Quantification
import org.clulab.wm.eidos.groundings.AdjectiveGrounder
import org.clulab.wm.eidos.groundings.AdjectiveGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DomainParams

case class GroundedEntity(sentence: String,
  quantifier: Quantifier,
  entity: Entity,
  predictedDelta: Option[Double],
  mean: Option[Double],
  stdev: Option[Double])

class EntityGrounder(val adjectiveGrounder: AdjectiveGrounder, val domainParams: DomainParams) {

  def groundEntities(mentions: Seq[EidosMention]): Seq[GroundedEntity] = {
    val gms = for {
      m <- mentions
      (quantifications, increases, decreases) = separateAttachments(m)

      groundedQuantifications = for {
        q <- quantifications
        quantTrigger = q.asInstanceOf[Quantification].trigger
      } yield groundEntity( m.odinMention, quantTrigger)

      groundedIncreases = for {
        inc <- increases
        quantTrigger <- inc.asInstanceOf[Increase].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity( m.odinMention, quantTrigger)

      groundedDecreases = for {
        dec <- decreases
        quantTrigger <- dec.asInstanceOf[Decrease].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m.odinMention, quantTrigger)


    } yield groundedQuantifications ++ groundedIncreases ++ groundedDecreases

    gms.flatten.toVector
  }

  def groundEntity(mention: Mention, quantifier: String): GroundedEntity = {
    // add the calculation
//    println("loaded domain params:" + domainParams.toString())
//    println(s"\tkeys: ${domainParams.keys.mkString(", ")}")
//    println(s"getting details for: ${mention.text}")

    val paramDetails: Map[String, Double] = domainParams.get(DomainParams.DEFAULT_DOMAIN_PARAM).get
    val paramMean = paramDetails(DomainParams.PARAM_MEAN)
    val paramStdev = paramDetails(DomainParams.PARAM_STDEV)
    val grounding = adjectiveGrounder.groundAdjective(quantifier).getOrElse(AdjectiveGrounding(None, None, None))
    val predictedDelta = grounding.predictDelta(paramMean, paramStdev)

    GroundedEntity(mention.document.sentences(mention.sentence).getSentenceText, quantifier, mention.text, predictedDelta, grounding.mu, grounding.sigma)
  }

  // Return the sorted Quantification, Increase, and Decrease modifications
  def separateAttachments(m: EidosMention): (Set[Attachment], Set[Attachment], Set[Attachment]) = {
    val attachments = m.odinMention.attachments
    (attachments.filter(_.isInstanceOf[Quantification]),
        attachments.filter(_.isInstanceOf[Increase]),
        attachments.filter(_.isInstanceOf[Decrease]))
  }
}
