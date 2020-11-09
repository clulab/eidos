package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.{OntologyAliases, OntologyGrounding}
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  def getGroundingOpt(groundings: OntologyAliases.OntologyGroundings, namespace: String): Option[OntologyGrounding] = {
    groundings.get(namespace)
  }

  def getGroundingOpt(mention: EidosMention, namespace: String): Option[OntologyGrounding] =
      getGroundingOpt(mention.grounding, namespace)

  def getBaseGroundingOpt(mention: EidosMention): Option[OntologyGrounding] =
      getGroundingOpt(mention, EidosOntologyGrounder.PRIMARY_NAMESPACE)

  def getBaseGroundingOpt(groundings: OntologyAliases.OntologyGroundings): Option[OntologyGrounding] =
      getGroundingOpt(groundings, EidosOntologyGrounder.PRIMARY_NAMESPACE)

  // Get the top primary (formerly UN) ontology grounding.
  def getBaseGroundingStringOpt(mention: EidosMention): Option[String] = {
    val groundingOpt = getBaseGroundingOpt(mention)

    groundingOpt.flatMap(_.headName)
  }

  def getBaseGroundingString(mention: EidosMention): String =
      getBaseGroundingStringOpt(mention).getOrElse("(unavailable)")

  // Get the top k groundings from the desired ontology (identified by namespace: String), with scores.
  def getGroundingsStringOpt(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): Option[String] = {
//    val groundingOpt = getGroundingOpt(mention, namespace)
    val groundingOpt = mention.grounding.headOption.map(_._2)

    groundingOpt.map(_.take(topK).mkString(delim))
  }

  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): String =
      getGroundingsStringOpt(mention, namespace, topK, delim).getOrElse("(namespace unavailable)")

  def noisyOr(values: Seq[Float], scale: Float = 1.0f): Float = {
    val epsilon = 0.01f
    val result = 1.0f - values.fold(1.0f)((product, value) => product * (1.0f - (value - epsilon)))
    result * scale
  }
}
