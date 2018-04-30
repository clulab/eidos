package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention

case class OntologyGrounding(grounding: Seq[(String, Double)])

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
}

class EidosOntologyGrounder {

  // Be careful, because object may not be completely constructed.
  def groundOntology(mention: EidosMention, wordToVec: EidosWordToVec): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      // Make vector for canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts))
    }
    else
      OntologyGrounding(Seq.empty)
  }

}

object EidosOntologyGrounder {
  def apply() = new EidosOntologyGrounder()
}