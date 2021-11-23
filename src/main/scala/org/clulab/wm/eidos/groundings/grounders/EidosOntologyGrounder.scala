package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.ConceptPatterns
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.IndividualGrounding
import org.clulab.wm.eidos.groundings.OntologyAliases
import org.clulab.wm.eidos.groundings.OntologyAliases.IndividualGroundings
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundingMap
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.OntologyNodeGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosTokenizer
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.ontologies.DomainOntology

import scala.util.matching.Regex

abstract class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends OntologyGrounder {

  def newOntologyGrounding(grounding: OntologyAliases.IndividualGroundings = Seq.empty, branch: Option[String] = None): OntologyGrounding = {
    OntologyGrounding(domainOntology.version, domainOntology.date, grounding, branch)
  }

  // TODO: These may have to change depending on whether n corresponds to leaf or branch node.
  val conceptEmbeddings: Seq[ConceptEmbedding] =
    domainOntology.nodes.map { node =>
      val negValues = node.getNegValues
      ConceptEmbedding(
        node,
        wordToVec.makeCompositeVector(node.getValues),
        if (negValues.nonEmpty) Some(wordToVec.makeCompositeVector(negValues)) else None
      )
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    domainOntology.nodes.map { node =>
      ConceptPatterns(node, node.getPatternsOpt)
    }

  // For API to reground strings
  def groundText(mentionText: String, canonicalNameParts: Array[String]): OntologyGrounding = {
    // Sieve-based approach
    newOntologyGrounding(groundPatternsThenEmbeddings(mentionText, canonicalNameParts, conceptPatterns, conceptEmbeddings))
  }

  def groundable(mention: EidosMention, primaryGrounding: Option[OntologyGroundingMap]): Boolean = EidosOntologyGrounder.groundableType(mention)

  // For Regex Matching
  def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[OntologyNodeGrounding] = {
    nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => OntologyNodeGrounding(node.namer, 1.0f))
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

  def groundPatternsThenEmbeddings(text: String, patterns: Seq[ConceptPatterns], embeddings: Seq[ConceptEmbedding]): IndividualGroundings = {
    groundPatternsThenEmbeddings(text, text.split(" +"), patterns, embeddings)
  }
  def groundPatternsThenEmbeddings(splitText: Array[String], patterns: Seq[ConceptPatterns], embeddings: Seq[ConceptEmbedding]): IndividualGroundings = {
    groundPatternsThenEmbeddings(splitText.mkString(" "), splitText, patterns, embeddings)
  }
  def groundPatternsThenEmbeddings(text: String, splitText: Array[String], patterns: Seq[ConceptPatterns], embeddings: Seq[ConceptEmbedding]): IndividualGroundings = {
    val lowerText = text.toLowerCase
    val exactMatches = embeddings.filter(embedding => StringUtils.afterLast(embedding.namer.getName, '/', true) == lowerText)
    if (exactMatches.nonEmpty)
      exactMatches.map(exactMatch => OntologyNodeGrounding(exactMatch.namer, 1.0f))
    else {
      val matchedPatterns = nodesPatternMatched(text, patterns)
      if (matchedPatterns.nonEmpty)
        matchedPatterns
      else {
        // Otherwise, back-off to the w2v-based approach
        // TODO: The line below only uses the positive embeddings, not the negative ones.
        // Should something be done there or here to take them into account.
        wordToVec.calculateSimilarities(splitText, embeddings).map(OntologyNodeGrounding(_))
      }
    }
  }

  /**
   * Removes all groundings below a provided threshold (if provided), then truncates the remaining
   * groundings if there are more than asked for (if topN provided)
   * @param fullGrounding the multiple groundings to different nodes in the ontology
   * @param topNOpt optional number of groundings you want to return
   * @param thresholdOpt optional threshold for the grounding score, below which you want to prune
   * @return surviving groundings
   */
  def filterAndSlice(fullGrounding: IndividualGroundings, topNOpt: Option[Int] = None, thresholdOpt: Option[Float] = None): IndividualGroundings = {
    val filtered = thresholdOpt.map { threshold =>
      fullGrounding.filter { case i: IndividualGrounding => i.score >= threshold }
    }.getOrElse(fullGrounding)
    val sorted = filtered.sortBy(grounding => -grounding.score)
    val taken = topNOpt.map { topN =>
      sorted.take(topN)
    }.getOrElse(sorted)

    taken
  }
}

object EidosOntologyGrounder extends Logging {
  val GROUNDABLE = "Entity"

  protected val               WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  protected val WM_COMPOSITIONAL_NAMESPACE = "wm_compositional" // As of now, the compositional 2.1 ontology uses this as the namespace
  protected val     WM_FLATTENED_NAMESPACE = "wm_flattened" // This one isn't in-house, but for completeness...
  protected val               UN_NAMESPACE = "un"
  protected val              WDI_NAMESPACE = "wdi"
  protected val              FAO_NAMESPACE = "fao"
  protected val             MESH_NAMESPACE = "mesh"
  protected val            PROPS_NAMESPACE = "props"
  protected val          MITRE12_NAMESPACE = "mitre12"
  protected val              WHO_NAMESPACE = "who"
  protected val    INTERVENTIONS_NAMESPACE = "interventions"
  protected val            ICASA_NAMESPACE = "icasa"
  protected val          WM_FLAT_NAMESPACE = "wm_flat"
  protected val             MAAS_NAMES     = Set("MaaS-model", "MaaS-parameter", "MaaS-variable")

  //val PRIMARY_NAMESPACE: String = WM_FLATTENED_NAMESPACE // Assign the primary namespace here, publically.
  val PRIMARY_NAMESPACE: String = WM_COMPOSITIONAL_NAMESPACE // Assign the primary namespace here, publically.

  val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE) ++ MAAS_NAMES

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosOntologyGrounder = {
    new FlatOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
  }

  def mkGrounder(ontologyName: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer, tokenizer: EidosTokenizer): OntologyGrounder = {
    ontologyName match {
      //case WM_COMPOSITIONAL_NAMESPACE => new CompositionalGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case WM_COMPOSITIONAL_NAMESPACE => new SRLCompositionalGrounder(ontologyName, domainOntology, w2v, canonicalizer, tokenizer)
      case INTERVENTIONS_NAMESPACE => new InterventionGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case WM_FLAT_NAMESPACE => new InterventionSieveGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case _ => EidosOntologyGrounder(ontologyName, domainOntology, w2v, canonicalizer)
    }
  }
}
