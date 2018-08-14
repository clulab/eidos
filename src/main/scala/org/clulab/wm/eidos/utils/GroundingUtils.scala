package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  // Get the top UN ontology grounding.
  def getBaseGrounding(mention: EidosMention): String = {
    val namespace = EidosOntologyGrounder.UN_NAMESPACE

    if (mention.grounding.contains(namespace) && mention.grounding(namespace).nonEmpty)
      mention.grounding(namespace).head._1
    else
      "(unavailable)"
  }

  // Get the top k groundings from the desired ontology (identified by namespace: String), with scores.
  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): String = {
    if (mention.grounding.contains(namespace))
      mention.grounding(namespace).take(topK).mkString(delim)
    else
      "(namespace unavailable)"
  }
}
