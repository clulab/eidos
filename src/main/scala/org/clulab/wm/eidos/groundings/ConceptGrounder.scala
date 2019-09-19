package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.PostProcessing
import org.clulab.wm.eidos.groundings.ConceptAliases._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object ConceptAliases {
  type SingleConceptGrounding = (Namer, Float)
  type MultipleConceptGrounding = Seq[SingleConceptGrounding]
  // The string is something like wm or un.
  type ConceptGroundings = Map[String, ConceptGrounding]
}

case class CoreGrounding(val coreConcept: String, val groundings: MultipleConceptGrounding = Seq.empty)

case class ModifierGrounding(val modifier: String, val groundings: MultipleConceptGrounding = Seq.empty)

case class CompositeGrounding(val coreGrounding: CoreGrounding, modifierGroundings: Seq[ModifierGrounding] = Seq.empty)

case class ConceptGrounding(compositeGrounding: Option[CompositeGrounding]) {
  def nonEmpty: Boolean = compositeGrounding.nonEmpty
}

trait ConceptGrounder {
  def groundConcept(mention: EidosMention): ConceptGrounding
  def isGroundable(mention: EidosMention): Boolean
}

// It is unfortunate that the "ing" suffix is already part of grounding, so we're left with "er" even for a trait.
trait MultiConceptGrounder {
  def groundConcept(mention: EidosMention): ConceptGroundings
}

class EidosConceptGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer) extends ConceptGrounder {

  def groundConcept(mention: EidosMention): ConceptGrounding = {
    val coreConceptOpt: Option[String] = Some("hello")
    val conceptGroundingOpt: Option[CompositeGrounding] = coreConceptOpt.map { coreConcept =>
      val coreConceptGroundings: Option[MultipleConceptGrounding] = coreConceptOpt.map { coreConcept =>
        Seq(
          (domainOntology.getNamer(0), 0f),
          (domainOntology.getNamer(1), 1f),
          (domainOntology.getNamer(2), 2f),
          (domainOntology.getNamer(3), 3f),
          (domainOntology.getNamer(4), 4f)
        )
      }
      val coreGrounding = CoreGrounding(coreConcept, coreConceptGroundings.get)
      val modifiers: Seq[String] = Seq("one", "two", "three")
      val modifierGroundings: Seq[ModifierGrounding] = modifiers.map { modifier =>
        val modifierGroundings = Seq(
          (domainOntology.getNamer(0), 0f),
          (domainOntology.getNamer(1), 1f),
          (domainOntology.getNamer(2), 2f),
          (domainOntology.getNamer(3), 3f),
          (domainOntology.getNamer(4), 4f)
        )

        ModifierGrounding(modifier, modifierGroundings)
      }

      CompositeGrounding(coreGrounding, modifierGroundings)
    }

    ConceptGrounding(conceptGroundingOpt)
  }

  def isGroundable(mention: EidosMention): Boolean = EidosConceptGrounder.groundableType(mention)
}

class EidosMultiConceptGrounder(conceptGrounders: Seq[EidosConceptGrounder]) extends MultiConceptGrounder with PostProcessing {

  def groundConcept(mention: EidosMention): ConceptGroundings = {
    val groundings = conceptGrounders.map { conceptGrounder =>
      (conceptGrounder.name, conceptGrounder.groundConcept(mention))
    }.toMap

    groundings
  }

  def process(annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    annotatedDocument.allEidosMentions.foreach(groundConcept)
    annotatedDocument
  }
}

object EidosConceptGrounder {
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

  val PRIMARY_NAMESPACE: String = WM_NAMESPACE // Assign the primary namespace here, publically.

  // Used for plugin ontologies
//  protected val INTERVENTION_PLUGIN_TRIGGER = "UN/interventions"
  protected val INTERVENTION_PLUGIN_TRIGGER = "wm/concept/causal_factor/intervention/"

  protected val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosConceptGrounder =
      new EidosConceptGrounder(name, domainOntology, wordToVec, canonicalizer)
}
