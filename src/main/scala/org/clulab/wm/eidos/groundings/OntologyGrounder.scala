package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import org.clulab.wm.eidos.attachments.{EidosAttachment, Property}
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.GroundingUtils
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

object Aliases {
  type SingleGrounding = (Namer, Float)
  type MultipleGrounding = Seq[SingleGrounding]
  // This now has to store the version information as well as the mapping from name to grounding.
  type Groundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(version: Option[String], date: Option[ZonedDateTime], grounding: Aliases.MultipleGrounding = Seq.empty) {
  def nonEmpty: Boolean = grounding.nonEmpty

  def take(n: Int): Aliases.MultipleGrounding = grounding.take(n)
  def headOption: Option[Aliases.SingleGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_._1.name)
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

trait MultiOntologyGrounding {
  def groundOntology(mention: EidosMention): Aliases.Groundings
}


class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends OntologyGrounder {
  // Is not dependent on the output of other grounders
  val isPrimary = true

  def newOntologyGrounding(grounding: Aliases.MultipleGrounding = Seq.empty) = {
    OntologyGrounding(domainOntology.version, domainOntology.date, grounding)
  }

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
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        newOntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        newOntologyGrounding(wordToVec.calculateSimilarities(mention.canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      newOntologyGrounding()
  }

  def groundable(mention: EidosMention, primaryGrounding: Option[Aliases.Groundings]): Boolean = EidosOntologyGrounder.groundableType(mention)

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
      newOntologyGrounding(matchedPatterns)
    }
    // Otherwise, back-off to the w2v-based approach
    else {
      newOntologyGrounding(wordToVec.calculateSimilarities(text.split(" +"), conceptEmbeddings))
    }
  }

}

// todo: surely there is a way to unify this with the PluginOntologyGrounder below -- maybe split out to a "stringMatchPlugin" and an "attachmentBasedPlugin" ?
class PropertiesOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec) extends EidosOntologyGrounder(name, domainOntology, wordToVec) {

  override def groundable(mention: EidosMention, primaryGrounding: Option[Aliases.Groundings]): Boolean = EidosOntologyGrounder.groundableType(mention) && mention.odinMention.attachments.exists(a => a.isInstanceOf[Property])

  override def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding = {
    if (groundable(mention, previousGroundings)) {
      val propertyAttachments = mention.odinMention.attachments.filter(a => a.isInstanceOf[Property])
      // These need to be sorted after retrieval from a set.  Otherwise the order differs and
      // eventual multiplication of floats in different orders produces different results.
      val propertyTokens = propertyAttachments.flatMap(EidosAttachment.getAttachmentWords).toArray.sorted

      // FIXME - should be lemmas?
      newOntologyGrounding(wordToVec.calculateSimilarities(propertyTokens, conceptEmbeddings))
    }
    else
      newOntologyGrounding()
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
    val groundable = previousGrounding match {
      case Some(prev) =>
        val groundingOpt: Option[OntologyGrounding] = GroundingUtils.getBaseGroundingOpt(prev)
        val containsPluginGroundingTrigger = groundingOpt.exists { grounding =>
          grounding
              .headName
              .map (_ contains pluginGroundingTrigger)
              .getOrElse(false)
        }
        containsPluginGroundingTrigger
      case _ => false
    }

    groundable
  }

  override def groundOntology(mention: EidosMention, previousGroundings: Option[Aliases.Groundings]): OntologyGrounding = {
    if (groundable(mention, previousGroundings)) {
      super.groundOntology(mention, None)
    } else {
      newOntologyGrounding()
    }
  }
}

class MultiOntologyGrounder(ontologyGrounders: Seq[EidosOntologyGrounder]) extends MultiOntologyGrounding {
  // Some plugin grounders need to be run after the primary grounders, i.e., they depend on the output of the primary grounders
  protected val (primaryGrounders, secondaryGrounders) = ontologyGrounders.partition(_.isPrimary)

  def groundOntology(mention: EidosMention): Groundings = {
    val primaryGroundings = primaryGrounders.map(ontologyGrounder =>
      (ontologyGrounder.name, ontologyGrounder.groundOntology(mention))).toMap
    val secondaryGroundings = secondaryGrounders.map(ontologyGrounder =>
      (ontologyGrounder.name, ontologyGrounder.groundOntology(mention, primaryGroundings))).toMap

    primaryGroundings ++ secondaryGroundings
  }
}

object EidosOntologyGrounder {
  protected val        GROUNDABLE = "Entity"
  protected val      WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  // Namespace strings for the different in-house ontologies we typically use
  protected val      UN_NAMESPACE = "un"
  protected val     WDI_NAMESPACE = "wdi"
  protected val     FAO_NAMESPACE = "fao"
  protected val    MESH_NAMESPACE = "mesh"
  protected val   PROPS_NAMESPACE = "props"
  protected val MITRE12_NAMESPACE = "mitre12"
  protected val     WHO_NAMESPACE = "who"
  protected val     INT_NAMESPACE = "interventions"
  protected val   ICASA_NAMESPACE = "icasa"

  val PRIMARY_NAMESPACE = WM_NAMESPACE // Assign the primary namespace here, publically.

  // Used for plugin ontologies
//  protected val INTERVENTION_PLUGIN_TRIGGER = "UN/interventions"
  protected val INTERVENTION_PLUGIN_TRIGGER = "wm/concept/causal_factor/intervention/"

  val indicatorNamespaces = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

  protected lazy val logger = LoggerFactory.getLogger(this.getClass())

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec): EidosOntologyGrounder =
    name match {
      case INT_NAMESPACE => new PluginOntologyGrounder(name, domainOntology, wordToVec, INTERVENTION_PLUGIN_TRIGGER)
      case PROPS_NAMESPACE => new PropertiesOntologyGrounder(name, domainOntology, wordToVec)
      case _ => new EidosOntologyGrounder(name, domainOntology, wordToVec)
    }
}
