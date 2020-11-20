package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.groundings.{EidosWordToVec, OntologyGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Canonicalizer
import org.clulab.wm.ontologies.DomainOntology

// TODO: Zupon
@deprecated("This grounder is deprecated", "2020-08-11")
class InterventionGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
// TODO This might extend something else
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddings).map(SingleOntologyNodeGrounding(_)), Some("intervention")))
  }

  def groundEidosMention(mention: EidosMention, topN: Option[Int] = Option(5), threshold: Option[Float] = Option(0.5f)): Seq[OntologyGrounding] = {
    val attachmentWords = mention.odinMention.attachments.flatMap(a => EidosAttachment.getAttachmentWords(a))
    val canonicalNameParts = EidosMention.canonicalNameParts(canonicalizer, mention, attachmentWords)

    groundStrings(canonicalNameParts)
  }
}
