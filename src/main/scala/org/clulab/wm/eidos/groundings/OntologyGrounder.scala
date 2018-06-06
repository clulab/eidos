package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention

object Aliases {
  type Grounding = Seq[(String, Double)]
  type Groundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: Aliases.Grounding = Seq.empty)

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
}

trait MultiOntologyGrounder {
  def groundOntology(mention: EidosMention): Aliases.Groundings
}

class EidosOntologyGrounder(var name: String, conceptEmbeddings: Seq[ConceptEmbedding], wordToVec: EidosWordToVec) extends OntologyGrounder {

  def groundOntology(mention: EidosMention): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
    }
    else
      OntologyGrounding()
  }
}

object EidosOntologyGrounder {

  def apply(domainOntology: DomainOntology, wordToVec: EidosWordToVec) = {
    val conceptEmbeddings = domainOntology.iterateOntology(wordToVec)

    new EidosOntologyGrounder(domainOntology.name, conceptEmbeddings, wordToVec)
  }
}
