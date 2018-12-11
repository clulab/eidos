package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.attachments.{EidosAttachment, Property}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{Namer, Sourcer}
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

object Aliases {
  type SingleGrounding = (Namer, Float)
  type MultipleGrounding = Seq[SingleGrounding]
  type Groundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: Aliases.MultipleGrounding = Seq.empty) {
  def nonEmpty: Boolean = grounding.nonEmpty

  def take(n: Int): Aliases.MultipleGrounding = grounding.take(n)
  def head: Aliases.SingleGrounding = grounding.head
}

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
}

trait MultiOntologyGrounder {
  def groundOntology(mention: EidosMention): Aliases.Groundings
}

class EidosOntologyGrounder(val name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends OntologyGrounder {

  val conceptEmbeddings: Seq[ConceptEmbedding] =
    0.until(domainOntology.size).map { n =>
      new ConceptEmbedding(domainOntology.getNamer(n),
          wordToVec.makeCompositeVector(domainOntology.getValues(n)))
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    0.until(domainOntology.size).map { n =>
      new ConceptPatterns(domainOntology.getNamer(n),
        domainOntology.getPatterns(n))
    }

  def groundOntology(mention: EidosMention): OntologyGrounding = {

    def nodePatternsMatch(s: String, patterns: Option[Seq[Regex]]): Boolean = {
      patterns match {
        case None => false
        case Some(rxs) =>
          for (r <- rxs) {
            if (r.findFirstIn(s).nonEmpty) return true
          }
          false
      }
    }

    def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[(Namer, Float)] = {
      nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => (node.namer, 1.0f))
    }

    // Sieve-based approach
    if (mention.odinMention.matches(EidosOntologyGrounder.GROUNDABLE)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        OntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalName = mention.canonicalName
        val canonicalNameParts = canonicalName.split(" +")
        OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      OntologyGrounding()
  }
}

class PropertiesOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(name, domainOntology, wordToVec) {

  override def groundOntology(mention: EidosMention): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val propertyAttachments = mention.odinMention.attachments.filter(a => a.isInstanceOf[Property])
      // These need to be sorted after retrieval from a set.  Otherwise the order differs and
      // eventual multiplication of floats in different orders produces different results.
      val propertyTokens = propertyAttachments.flatMap(EidosAttachment.getAttachmentWords).toArray.sorted

      // FIXME - replaced conceptEmbeddings with conceptEmbeddingsAll
      OntologyGrounding(wordToVec.calculateSimilarities(propertyTokens, conceptEmbeddings))
    }
    else
      OntologyGrounding()
  }
}

object EidosOntologyGrounder {
  val        GROUNDABLE = "Entity"
  // Namespace strings for the different in-house ontologies we typically use
  val      UN_NAMESPACE = "un"
  val     WDI_NAMESPACE = "wdi"
  val     FAO_NAMESPACE = "fao"
  val    MESH_NAMESPACE = "mesh"
  val   PROPS_NAMESPACE = "props"
  val MITRE12_NAMESPACE = "mitre12"
  val     WHO_NAMESPACE = "who"

  val indicatorNamespaces = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE)

  protected val logger = LoggerFactory.getLogger(this.getClass())

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec): EidosOntologyGrounder =
    name match {
      case PROPS_NAMESPACE => new PropertiesOntologyGrounder(name, domainOntology, wordToVec)
      case _ => new EidosOntologyGrounder(name, domainOntology, wordToVec)
    }
}
