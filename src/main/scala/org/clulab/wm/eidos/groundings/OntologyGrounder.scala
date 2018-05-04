package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DomainOntology, FileUtils, Sourcer}

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

class EidosOntologyGrounder(var name: String, domainOntoPath: String, wordToVec: EidosWordToVec) extends OntologyGrounder {
  val conceptEmbeddings = EidosOntologyGrounder.getConceptEmbeddings(domainOntoPath, wordToVec)

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

class DomainOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec) {
  // Override methods here
}

class UNOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec) {
  // Override methods here
}

class WDIOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec) {
  // Override methods here
}

class FAOOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec) {
  // Override methods here
}

object EidosOntologyGrounder {

  // Namespace strings for the different in-house ontologies we typically use
  val UN_NAMESPACE = "un"
  val WDI_NAMESPACE = "wdi"
  val FAO_NAMESPACE = "fao"

  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath))

    ontology.iterateOntology(wordToVec)
  }
}
