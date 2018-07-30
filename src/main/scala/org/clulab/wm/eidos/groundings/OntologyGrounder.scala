package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention
import org.slf4j.LoggerFactory

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

class EidosOntologyGrounder(var name: String, conceptEmbeddings: Seq[ConceptEmbedding], wordToVec: EidosWordToVec) extends OntologyGrounder with Serializable {

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
  // Namespace strings for the different in-house ontologies we typically use
  val UN_NAMESPACE = "un"
  val WDI_NAMESPACE = "wdi"
  val FAO_NAMESPACE = "fao"

  protected val logger = LoggerFactory.getLogger(this.getClass())

  def apply(domainOntology: DomainOntology, wordToVec: EidosWordToVec) = {
    logger.info(s"Processing ${domainOntology.name} ontology...")
    val conceptEmbeddings = domainOntology.iterateOntology(wordToVec)

    new EidosOntologyGrounder(domainOntology.name, conceptEmbeddings, wordToVec)
  }

//  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
//    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos = false)
//
//    ontology.iterateOntology(wordToVec)
  }
