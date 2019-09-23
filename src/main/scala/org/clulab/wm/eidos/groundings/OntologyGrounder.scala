package org.clulab.wm.eidos.groundings

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.struct.Interval
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object Aliases {
  type SingleGrounding = (Namer, Float)
  type MultipleGrounding = Seq[SingleGrounding]
  // The string is something like wm or un.
  type Groundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: Aliases.MultipleGrounding = Seq.empty) {
  def nonEmpty: Boolean = grounding.nonEmpty
  def take(n: Int): Aliases.MultipleGrounding = grounding.take(n)
  def headOption: Option[Aliases.SingleGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_._1.name)
}

trait OntologyGrounder {
  def name: String
  def domainOntology: DomainOntology
  def groundOntology(mention: EidosMention): OntologyGrounding
  def updateGrounding(mention: EidosMention): Unit = {
    val g = groundOntology(mention)
    if (g.grounding.nonEmpty) { // if it grounded to something then attach to mention
      val newGroundings = mention.groundings match {
        case None => Map(name -> g)
        case Some(gs) => gs.updated(name, g)
      }
      mention.groundings = Some(newGroundings)
    }
  }
}

class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer) extends OntologyGrounder {
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

  def groundOntology(mention: EidosMention): OntologyGrounding = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        OntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
        OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      OntologyGrounding()
  }

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

class CompositionalGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer) extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  override def groundOntology(mention: EidosMention): OntologyGrounding = {
    // Separate ontology nodes into Process, Property, and Phenomenon
    val (processNodes, other) = conceptEmbeddings.partition(_.namer.name.contains("wm/process"))
    val (propertyNodes, phenomenonNodes) = other.partition(_.namer.name.contains("wm/property"))

    // TODO: Andrew, please fill in!
    //  split mention into syntactic head and modifiers based on dependencies?
    //  head tends to be the property (e.g. PRICE in 'oil price' and 'price of oil')
    //  ground only >compound and >nmod_ ?



    /** start by grounding syntactic head */

    // make a new mention that's just the syntactic head of the original mention
    val syntacticHead = mention.odinMention.synHead
    val mentionHead = new ArrayBuffer[Mention]
    for {
      tok <- syntacticHead
    } mentionHead.append(
      new TextBoundMention(
        Seq("Mention_head"),
        tokenInterval = Interval(tok),
        sentence = mention.odinMention.sentence,
        document = mention.odinMention.document,
        keep = mention.odinMention.keep,
        foundBy = mention.odinMention.foundBy
      )
    )

    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(EidosMention.asEidosMentions(mentionHead).head)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
    val matchedPatterns = nodesPatternMatched(mentionHead.head.text, conceptPatterns)
    if (matchedPatterns.nonEmpty) {
      OntologyGrounding(matchedPatterns)
    }
      // Otherwise, back-off to the w2v-based approach
    else {
      val canonicalNameParts = canonicalizer.canonicalNameParts(EidosMention.asEidosMentions(mentionHead).head)
      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
    }
//    else
//      OntologyGrounding()

      //todo: how to look inside grounding to see which partition it is part of?



    // Get all dependencies within the original mention
    val dependencies = mention.odinMention.sentenceObj.dependencies

    // Get outgoing dependencies
    val outgoing = dependencies match {
      case Some(deps) => deps.outgoingEdges
      case None => Array.empty
    }

    // Get incoming dependencies
    val incoming = dependencies match {
      case Some(deps) => deps.incomingEdges
      case None => Array.empty
    }



  /** then ground modifiers of syntactic head */

    // Get modifiers in original mention

    val modifiers = new ArrayBuffer[Mention]

    val argumentIntervals = mention.odinMention.arguments.values.flatten.map(_.tokenInterval)
    for {
      tok <- mention.odinMention.tokenInterval
      if !argumentIntervals.exists(_.contains(tok))
      in <- incoming.lift(tok)
      (ix, label) <- in
      if label == "compound" || label == "nmod_of"
    } modifiers.append(
      new TextBoundMention(
        Seq("Compositional_modifier"),
        Interval(tok),  // todo: not sure if this should be 'tok'
        sentence = mention.odinMention.sentence,
        document = mention.odinMention.document,
        keep = mention.odinMention.keep,
        foundBy = mention.odinMention.foundBy
      )
    )


    ???
  }
}

class InterventionGrounder(val name: String, val domainOntology: DomainOntology, val w2v: EidosWordToVec, val canonicalizer: Canonicalizer) extends OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding = ???


}

object EidosOntologyGrounder {
  protected val                 GROUNDABLE = "Entity"
  protected val               WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  protected val WM_COMPOSITIONAL_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  // Namespace strings for the different in-house ontologies we typically use
  protected val               UN_NAMESPACE = "un"
  protected val              WDI_NAMESPACE = "wdi"
  protected val              FAO_NAMESPACE = "fao"
  protected val             MESH_NAMESPACE = "mesh"
  protected val            PROPS_NAMESPACE = "props"
  protected val          MITRE12_NAMESPACE = "mitre12"
  protected val              WHO_NAMESPACE = "who"
  protected val     INTERVENTION_NAMESPACE = "interventions"
  protected val            ICASA_NAMESPACE = "icasa"

  val PRIMARY_NAMESPACE: String = WM_NAMESPACE // Assign the primary namespace here, publically.

  protected val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosOntologyGrounder = {
    EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
  }

  def mkGrounder(ontologyName: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer): OntologyGrounder = {
    ontologyName match {
      case WM_COMPOSITIONAL_NAMESPACE => new CompositionalGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case INTERVENTION_NAMESPACE => new InterventionGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case _ => EidosOntologyGrounder(ontologyName, domainOntology, w2v, canonicalizer)
    }
  }



}
