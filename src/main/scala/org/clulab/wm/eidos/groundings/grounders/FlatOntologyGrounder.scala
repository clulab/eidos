package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.groundings.{EidosWordToVec, OntologyGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Canonicalizer
import org.clulab.wm.ontologies.DomainOntology

class FlatOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  // TODO Move some stuff from above down here if it doesn't apply to other grounders.

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(wordToVec.calculateSimilarities(strings, conceptEmbeddings).map(SingleOntologyNodeGrounding(_))))
  }

  def groundEidosMention(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // This assumes it to be highly unlikely that there will be an exact match or pattern match
      // because otherwise the canonicalNameParts are never used and they shouldn't be calculated.
      val attachmentWords = mention.odinMention.attachments.flatMap(a => EidosAttachment.getAttachmentWords(a))
      val canonicalNameParts = EidosMention.canonicalNameParts(canonicalizer, mention, attachmentWords)
      val aggregated = groundPatternsThenEmbeddings(mention.odinMention.text, canonicalNameParts, conceptPatterns, conceptEmbeddings)
      val filtered = filterAndSlice(aggregated, topN, threshold)
      Seq(newOntologyGrounding(filtered))
    }
    else
      Seq(newOntologyGrounding())
  }
}
