package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.groundings.DomainOntology
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer

class FlatOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  // TODO Move some stuff from above down here if it doesn't apply to other grounders.

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(wordToVec.calculateSimilarities(strings, conceptEmbeddings)))
  }

  def groundOntology(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        Seq(newOntologyGrounding(matchedPatterns))
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
        groundStrings(canonicalNameParts)
      }
    }
    else
      Seq(newOntologyGrounding())
  }
}
