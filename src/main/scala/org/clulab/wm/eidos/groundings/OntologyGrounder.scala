package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DomainOntology, FileUtils}

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

class EidosOntologyGrounder(var name: String, ontologyPath: String, wordToVec: EidosWordToVec, filterOnPos: Boolean) extends OntologyGrounder {
  protected val conceptEmbeddings =
      DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos).iterateOntology(wordToVec)

  def groundOntology(mention: EidosMention): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      // Make vector for canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
    }
    else
      OntologyGrounding()
  }
}

class DomainOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec, filterOnPos = false) {
  // This is the default grounder for when no other is specified.
  // It uses the file stored in domainOntologyPath.
}

class UNOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    // Note: No need to filter on POS tags as they contain examples
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec, filterOnPos = false) {
}

class WDIOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
    // Note: Filter on POS tags as they contain descriptions
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec, filterOnPos = true) {
}

class FAOOntologyGrounder(name: String, ontologyPath: String, wordToVec: EidosWordToVec)
//<<<<<<< HEAD
//    extends EidosOntologyGrounder(name, ontologyPath, wordToVec) {
//
//  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
//    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos = true) // Note: Filter on POS tags as they contain descriptions
//
//    ontology.iterateOntology(wordToVec)
//  }
//}
//
//object EidosOntologyGrounder {
//
//  // Namespace strings for the different in-house ontologies we typically use
//  val UN_NAMESPACE = "un"
//  val WDI_NAMESPACE = "wdi"
//  val FAO_NAMESPACE = "fao"
//
//  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
//    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos = false)
//
//    ontology.iterateOntology(wordToVec)
//  }
//=======
    // Note: Filter on POS tags as they contain descriptions
    extends EidosOntologyGrounder(name, ontologyPath, wordToVec, filterOnPos = true) {
//>>>>>>> master
}

object EidosOntologyGrounder {

  // Namespace strings for the different in-house ontologies we typically use
  val UN_NAMESPACE = "un"
  val WDI_NAMESPACE = "wdi"
  val FAO_NAMESPACE = "fao"

//  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
//    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos = false)
//
//    ontology.iterateOntology(wordToVec)
  }
