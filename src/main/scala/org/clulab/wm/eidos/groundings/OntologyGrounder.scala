package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.attachments.{EidosAttachment, Property}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Namer
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
  val isPrimary: Boolean

  def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding
  def groundOntology(mention: EidosMention): OntologyGrounding = groundOntology(mention, None)
  def groundOntology(mention: EidosMention, previousGroundings: Aliases.Groundings): OntologyGrounding = groundOntology(mention, Some(previousGroundings))

  def groundable(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): Boolean
  def groundable(mention: EidosMention): Boolean = groundable(mention, None)
  def groundable(mention: EidosMention, previousGroundings: Aliases.Groundings): Boolean = groundable(mention, Some(previousGroundings))

}

trait MultiOntologyGrounder {
  def groundOntology(mention: EidosMention): Aliases.Groundings
}


class EidosOntologyGrounder(val name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends OntologyGrounder {
  // Is not dependent on the output of other grounders
  val isPrimary = true

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

  def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding = {

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

    def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[(Namer, Float)] = {
      nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => (node.namer, 1.0f))
    }

    // Sieve-based approach
    if (groundable(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        OntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        OntologyGrounding(wordToVec.calculateSimilarities(mention.canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      OntologyGrounding()
  }

  def groundable(mention: EidosMention, primaryGrounding: Option[Aliases.Groundings]): Boolean = EidosOntologyGrounder.groundableType(mention)

}

// todo: surely there is a way to unify this with the PluginOntologyGrounder below -- maybe split out to a "stringMatchPlugin" and an "attachmentBasedPlugin" ?
class PropertiesOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(name, domainOntology, wordToVec) {

  override def groundable(mention: EidosMention, primaryGrounding: Option[Aliases.Groundings]): Boolean = super.groundable(mention) && mention.odinMention.attachments.exists(a => a.isInstanceOf[Property])

  override def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding = {
    if (groundable(mention)) {
      val propertyAttachments = mention.odinMention.attachments.filter(a => a.isInstanceOf[Property])
      // These need to be sorted after retrieval from a set.  Otherwise the order differs and
      // eventual multiplication of floats in different orders produces different results.
      val propertyTokens = propertyAttachments.flatMap(EidosAttachment.getAttachmentWords).toArray.sorted

      // FIXME - should be lemmas?
      OntologyGrounding(wordToVec.calculateSimilarities(propertyTokens, conceptEmbeddings))
    }
    else
      OntologyGrounding()
  }
}

/**
  * Used to make a secondary grounding ONLY IF the primary grounding matches the specified trigger
  * @param name name of the ontology
  * @param domainOntology the ontology to use
  * @param wordToVec the w2v to calculate the similarities
  * @param pluginGroundingTrigger the string to look for in the primary grounding
  */
class PluginOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, pluginGroundingTrigger: String) extends EidosOntologyGrounder(name, domainOntology, wordToVec) {

  // No, because it IS dependent on the output of other grounders
  override val isPrimary = false

  override def groundable(mention: EidosMention, previousGrounding: Option[Aliases.Groundings]): Boolean = {
    previousGrounding match {
      case Some(prev) =>
        prev.get(EidosOntologyGrounder.UN_NAMESPACE).exists(_.head._1.name contains pluginGroundingTrigger)
      case _ => false
    }
  }

  override def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding = {
    if (groundable(mention, previousGroundings)) {
      super.groundOntology(mention)
    } else {
      OntologyGrounding()
    }
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
  val     INT_NAMESPACE = "interventions"
  // Used for plugin ontologies
  val INTERVENTION_PLUGIN_TRIGGER = "UN/interventions"

  val indicatorNamespaces = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE)

  protected lazy val logger = LoggerFactory.getLogger(this.getClass())

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec): EidosOntologyGrounder =
    name match {
      case INT_NAMESPACE => new PluginOntologyGrounder(name, domainOntology, wordToVec, INTERVENTION_PLUGIN_TRIGGER)
      case PROPS_NAMESPACE => new PropertiesOntologyGrounder(name, domainOntology, wordToVec)
      case _ => new EidosOntologyGrounder(name, domainOntology, wordToVec)
    }
}
