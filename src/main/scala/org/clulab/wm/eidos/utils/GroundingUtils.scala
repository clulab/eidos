package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder}
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  // Get the top primary (formerly UN) ontology grounding.
  def getBaseGrounding(mention: EidosMention): String = {
    val namespace = EidosOntologyGrounder.PRIMARY_NAMESPACE

    mention.groundings.get(namespace)(0)
        .headName
        .getOrElse("(unavailable)")
  }

  // Get the top k groundings from the desired ontology (identified by namespace: String), with scores.
  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): String = {
    if (mention.groundings.get.contains(namespace))
      mention.groundings.get(namespace)(0).take(topK).mkString(delim)
    else
      "(namespace unavailable)"
  }
}
