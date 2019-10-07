package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object OntologyAliases {
  type SingleOntologyGrounding = (Namer, Float)
  type MultipleOntologyGrounding = Seq[SingleOntologyGrounding]
  // The first string is the name, something like wm or un.  The second is a branch/category.
  type OntologyGroundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: MultipleOntologyGrounding = Seq.empty, branch: Option[String] = None) {
  def nonEmpty: Boolean = grounding.nonEmpty
  def take(n: Int): MultipleOntologyGrounding = grounding.take(n)
  def headOption: Option[SingleOntologyGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_._1.name)
}

trait OntologyGrounder {
  def name: String
  def domainOntology: DomainOntology
  def groundOntology(mention: EidosMention): Seq[OntologyGrounding]
}

abstract class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends OntologyGrounder {

  val conceptEmbeddings: Seq[ConceptEmbedding] =
    0.until(domainOntology.size).map { n =>
      ConceptEmbedding(domainOntology.getNamer(n),
           wordToVec.makeCompositeVector(domainOntology.getValues(n)))
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    0.until(domainOntology.size).map { n =>
      ConceptPatterns(domainOntology.getNamer(n),
        domainOntology.getPatterns(n))
    }

  def groundable(mention: EidosMention, primaryGrounding: Option[OntologyGroundings]): Boolean = EidosOntologyGrounder.groundableType(mention)

  // For Regex Matching
  def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[(Namer, Float)] = {
    nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => (node.namer, 1.0f))
  }

  def nodePatternsMatch(s: String, patterns: Option[Array[Regex]]): Boolean = {
    patterns match {
      case None => false
      case Some(rxs) =>
        for (r <- rxs) {
          if (r.findFirstIn(s).nonEmpty) return true
        }
        false
    }
  }

  // For API to reground strings
  def groundText(text: String): OntologyGrounding = {
    val matchedPatterns = nodesPatternMatched(text, conceptPatterns)
    if (matchedPatterns.nonEmpty) {
      OntologyGrounding(matchedPatterns)
    }
    // Otherwise, back-off to the w2v-based approach
    else {
      OntologyGrounding(wordToVec.calculateSimilarities(text.split(" +"), conceptEmbeddings))
    }
  }
}

class FlatOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  // TODO Move some stuff from above down here if it doesn't apply to other grounders.

  def groundOntology(mention: EidosMention): Seq[OntologyGrounding] = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        Seq(OntologyGrounding(matchedPatterns))
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
        Seq(OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings)))
      }
    }
    else
      Seq(OntologyGrounding())
  }
}

// Zupon
class CompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  protected lazy val conceptEmbedddingsSeq: Seq[(String, Seq[ConceptEmbedding])] = {

    def getBranch(branch: String): (String, Seq[ConceptEmbedding]) =
        branch -> conceptEmbeddings.filter { _.namer.branch == Some(branch) }

    Seq(
      getBranch("process"),
      getBranch("property"),
      getBranch("phenomenon")
    ).filter { case (_, conceptEmbeddings) => conceptEmbeddings.nonEmpty }
  }

  override def groundOntology(mention: EidosMention): Seq[OntologyGrounding] = {
    val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

    conceptEmbedddingsSeq.map { case (branch, conceptEmbeddings) =>
      OntologyGrounding(w2v.calculateSimilarities(canonicalNameParts, conceptEmbeddings), Some(branch))
    }
  }
}

// Zupon
class InterventionGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
    // TODO This might extend something else
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  def groundOntology(mention: EidosMention): Seq[OntologyGrounding] = {
    val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

    Seq(OntologyGrounding(w2v.calculateSimilarities(canonicalNameParts, conceptEmbeddings), Some("intervention")))
  }
}

object EidosOntologyGrounder {
  protected val                 GROUNDABLE = "Entity"
  protected val               WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  protected val WM_COMPOSITIONAL_NAMESPACE = "wm_compositional"
  // Namespace strings for the different in-house ontologies we typically use
  protected val               UN_NAMESPACE = "un"
  protected val              WDI_NAMESPACE = "wdi"
  protected val              FAO_NAMESPACE = "fao"
  protected val             MESH_NAMESPACE = "mesh"
  protected val            PROPS_NAMESPACE = "props"
  protected val          MITRE12_NAMESPACE = "mitre12"
  protected val              WHO_NAMESPACE = "who"
  protected val    INTERVENTIONS_NAMESPACE = "interventions"
  protected val            ICASA_NAMESPACE = "icasa"

  val PRIMARY_NAMESPACE: String = WM_NAMESPACE // Assign the primary namespace here, publically.

  val indicatorNamespaces = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosOntologyGrounder = {
    new FlatOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
  }

  def mkGrounder(ontologyName: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer): OntologyGrounder = {
    ontologyName match {
      case WM_COMPOSITIONAL_NAMESPACE => new CompositionalGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case INTERVENTIONS_NAMESPACE => new InterventionGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case _ => EidosOntologyGrounder(ontologyName, domainOntology, w2v, canonicalizer)
    }
  }
}
