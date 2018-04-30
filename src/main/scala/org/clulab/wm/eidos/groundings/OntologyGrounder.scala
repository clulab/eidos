package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DomainOntology, FileUtils, Sourcer}

case class OntologyGrounding(grounding: Seq[(String, Double)] = Seq.empty)

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
}

class EidosOntologyGrounder(domainOntoPath: String, wordToVec: EidosWordToVec) extends OntologyGrounder {
  val conceptEmbeddings = EidosOntologyGrounder.getConceptEmbeddings(domainOntoPath, wordToVec)

  // Be careful, because object may not be completely constructed.
  def groundOntology(mention: EidosMention): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      // Make vector for canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
    }
    else
      OntologyGrounding(Seq.empty)
  }
}

class UNOntologyGrounder(ontologyPath: String, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(ontologyPath, wordToVec) {
  // Override methods here
}

class WDIOntologyGrounder(ontologyPath: String, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(ontologyPath, wordToVec) {
  // Override methods here
}

class FAOOntologyGrounder(ontologyPath: String, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(ontologyPath, wordToVec) {
  // Override methods here
}

object EidosOntologyGrounder {

  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath))

    ontology.iterateOntology(wordToVec.w2v)
  }
}