package org.clulab.wm.eidos.serialization.webapp

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Quantification
import org.clulab.wm.eidos.groundings.AdjectiveGrounder
import org.clulab.wm.eidos.groundings.AdjectiveGrounding
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.webapp.HomeController.eidosConfig
import org.clulab.wm.eidos.utils.DomainParams

case class GroundedEntity(sentence: String,
  quantifier: Quantifier,
  entity: Entity,
  predictedDelta: Option[Double],
  mean: Option[Double],
  stdev: Option[Double])

object GroundedEntity {
  val stanza = "adjectiveGrounder"

  val domainParams: DomainParams = DomainParams.fromConfig(eidosConfig.getConfig(stanza))
  val adjectiveGrounder: AdjectiveGrounder = EidosAdjectiveGrounder.fromConfig(eidosConfig.getConfig(stanza))


  def groundEntities(ieSystem: EidosSystem, mentions: Seq[EidosMention]): Vector[GroundedEntity] = {
    val gms = for {
      m <- mentions
      (quantifications, increases, decreases) = separateAttachments(m)

      groundedQuantifications = for {
        q <- quantifications
        quantTrigger = q.asInstanceOf[Quantification].trigger
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)

      groundedIncreases = for {
        inc <- increases
        quantTrigger <- inc.asInstanceOf[Increase].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)

      groundedDecreases = for {
        dec <- decreases
        quantTrigger <- dec.asInstanceOf[Decrease].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)


    } yield groundedQuantifications ++ groundedIncreases ++ groundedDecreases

    gms.flatten.toVector
  }

  def groundEntity(mention: Mention, quantifier: String, ieSystem: EidosSystem): GroundedEntity = {
    // add the calculation
    println("loaded domain params:" + domainParams.toString())
    println(s"\tkeys: ${domainParams.keys.mkString(", ")}")
    println(s"getting details for: ${mention.text}")

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
