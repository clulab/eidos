package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.groundings.{DomainOntology, EidosWordToVec, OntologyGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer

class FlatOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  // TODO Move some stuff from above down here if it doesn't apply to other grounders.

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(wordToVec.calculateSimilarities(strings, conceptEmbeddings).map(SingleOntologyNodeGrounding(_))))
  }

  def groundEidosMention(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
      val aggregated = groundPatternsThenEmbeddings(mention.odinMention.text, canonicalNameParts, conceptPatterns, conceptEmbeddings)
      val filtered = filterAndSlice(aggregated, topN, threshold)
      Seq(newOntologyGrounding(filtered))
    }
    else
      Seq(newOntologyGrounding())
  }
}
