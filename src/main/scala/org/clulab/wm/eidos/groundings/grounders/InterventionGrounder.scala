package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.groundings.DomainOntology
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer

// TODO: Zupon
class InterventionGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
// TODO This might extend something else
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddings), Some("intervention")))
  }

  def groundOntology(mention: EidosMention, topN: Option[Int] = Option(5), threshold: Option[Float] = Option(0.5f)): Seq[OntologyGrounding] = {
    val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

    groundStrings(canonicalNameParts)
  }
}
