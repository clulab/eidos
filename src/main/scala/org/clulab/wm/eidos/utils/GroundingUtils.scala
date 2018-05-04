package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  // Gets the top UN ontology grounding
  def getBaseGrounding(mention: EidosMention): String = {
    val allGroundings = mention.grounding
    val baseSet: OntologyGrounding = allGroundings(EidosOntologyGrounder.UN_NAMESPACE)
    baseSet.grounding.head._1
  }

  // Gets the top k groundings from the desired ontology (identified by namespace: String), with scores
  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5): String = {
    val grounding = mention.grounding(namespace)
    val topkGroundings = grounding.grounding.slice(0,topK)
    topkGroundings.mkString(", ")
  }

}
