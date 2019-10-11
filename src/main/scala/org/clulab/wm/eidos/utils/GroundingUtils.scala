package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.Aliases
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  def getGroundingOpt(groundings: Aliases.Groundings, namespace: String): Option[OntologyGrounding] = {
    groundings.get(namespace)
  }

  def getGroundingOpt(mention: EidosMention, namespace: String): Option[OntologyGrounding] =
      getGroundingOpt(mention.grounding, namespace)

  def getBaseGroundingOpt(mention: EidosMention): Option[OntologyGrounding] =
      getGroundingOpt(mention, EidosOntologyGrounder.PRIMARY_NAMESPACE)

  def getBaseGroundingOpt(groundings: Aliases.Groundings): Option[OntologyGrounding] =
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
    val groundingOpt = getGroundingOpt(mention, namespace)

    groundingOpt.map(_.take(topK).mkString(delim))
  }

  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): String =
      getGroundingsStringOpt(mention, namespace, topK, delim).getOrElse("(namespace unavailable)")
}
