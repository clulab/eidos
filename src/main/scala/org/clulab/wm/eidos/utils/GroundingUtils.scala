package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention

object GroundingUtils {

  // Get the top UN ontology grounding.
  def getBaseGrounding(mention: EidosMention): String = {
    val namespace = EidosOntologyGrounder.UN_NAMESPACE

    if (mention.grounding.contains(namespace)) {
      val grounding = mention.grounding(namespace)

      stripMetaDataType(grounding.grounding.head._1)
    }
    else
      "(unavailable)"
  }

  // Get the top k groundings from the desired ontology (identified by namespace: String), with scores.
  def getGroundingsString(mention: EidosMention, namespace: String, topK: Int = 5, delim: String = ", "): String = {
    if (mention.grounding.contains(namespace)) {
      val grounding = mention.grounding(namespace)
      val topkGroundings = grounding.grounding.take(topK).map(grd => (stripMetaDataType(grd._1), grd._2))
      topkGroundings.mkString(delim)
    }
    else "(namespace unavailable)"
  }

  // Strip the unwanted metadata suffixes.
  protected def stripMetaDataType(s: String): String = s.stripSuffix("/examples").stripSuffix("/description")
}
