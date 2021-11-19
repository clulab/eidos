package org.clulab.wm.eidos.groundings.grounders

import edu.stanford.nlp.util.EditDistance
import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.ConceptExamples
import org.clulab.wm.eidos.groundings.ConceptPatterns
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.IndividualGrounding
import org.clulab.wm.eidos.groundings.OntologyAliases
import org.clulab.wm.eidos.groundings.OntologyAliases.MultipleOntologyGrounding
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.SingleOntologyNodeGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosTokenizer
import org.clulab.wm.eidoscommon.utils.Collection
import org.clulab.wm.eidoscommon.utils.{Logging, Namer}
import org.clulab.wm.ontologies.DomainOntology

import scala.math.{log, pow}
import scala.util.matching.Regex

abstract class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends OntologyGrounder {

  def newOntologyGrounding(grounding: OntologyAliases.MultipleOntologyGrounding = Seq.empty, branchOpt: Option[String] = None): OntologyGrounding = {
    OntologyGrounding(domainOntology.version, domainOntology.date, grounding, branchOpt)
  }

  // This can be reused repeatedly.
  //def emptyOntologyGrounding(branchOpt: Option[String] = None) = new OntologyGrounding(domainOntology.version, domainOntology.date, branchOpt = branchOpt)
  val emptyOntologyGrounding: OntologyGrounding = OntologyGrounding(domainOntology.version, domainOntology.date)

  // TODO: These may have to change depending on whether n corresponds to leaf or branch node.
  val conceptEmbeddings: Seq[ConceptEmbedding] =
    domainOntology.indices.map { n =>
      val negValues = domainOntology.getNegValues(n)
      ConceptEmbedding(
        domainOntology.getNamer(n),
        wordToVec.makeCompositeVector(domainOntology.getValues(n)),
        if (negValues.nonEmpty) Some(wordToVec.makeCompositeVector(negValues)) else None
      )
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    domainOntology.indices.map { n =>
      ConceptPatterns(domainOntology.getNamer(n), domainOntology.getPatterns(n))
    }

  val conceptExamples: Seq[ConceptExamples] =
    domainOntology.indices.map { n =>
      ConceptExamples(domainOntology.getNamer(n), domainOntology.getExamples(n))
    }

  // For API to reground strings
  def groundText(mentionText: String, canonicalNameParts: Array[String]): OntologyGrounding = {
    // Sieve-based approach
    newOntologyGrounding(groundPatternsThenEmbeddings(mentionText, canonicalNameParts, conceptPatterns, conceptExamples, conceptEmbeddings))
  }

  def groundable(mention: EidosMention, primaryGrounding: Option[OntologyGroundings]): Boolean = EidosOntologyGrounder.groundableType(mention)

  // For Regex Matching
  def nodesPatternMatched(s: String, nodes: Seq[ConceptPatterns]): Seq[SingleOntologyNodeGrounding] = {
    nodes.filter(node => nodePatternsMatch(s, node.patterns)).map(node => SingleOntologyNodeGrounding(node.namer, 1.0f))
  }

  def nodePatternsMatch(string: String, patternsOpt: Option[Array[Regex]]): Boolean = {
    patternsOpt match {
      case None => false
      case Some(patterns) => patterns.exists(_.findFirstIn(string).nonEmpty)
    }
  }

  // For Regex Matching
  def nodesExampleMatched(string: String, nodes: Seq[ConceptExamples]): Map[Namer, Float] = {
    (
      for (node <- nodes)
      yield node.namer -> nodeExamplesMatch(string, node.examples)
    ).toMap
  }

  def nodeExamplesMatch(string: String, examples: Option[Array[String]]): Float = {
    examples match {
      case None => string.length.toFloat
      case Some(examples) =>
        val lowerString = string.toLowerCase // just once for all examples
        examples
            .map { example => new EditDistance().score(lowerString, example.toLowerCase) }
            .min
            .toFloat
    }
  }

  def groundPatternsThenEmbeddings(text: String, patterns: Seq[ConceptPatterns], examples: Seq[ConceptExamples], embeddings: Seq[ConceptEmbedding]): MultipleOntologyGrounding = {
    groundPatternsThenEmbeddings(text, text.split(" +"), patterns, examples, embeddings)
  }

  def groundPatternsThenEmbeddings(splitText: Array[String], patterns: Seq[ConceptPatterns], examples: Seq[ConceptExamples], embeddings: Seq[ConceptEmbedding]): MultipleOntologyGrounding = {
    groundPatternsThenEmbeddings(splitText.mkString(" "), splitText, patterns, examples, embeddings)
  }

  // If there was an exact match, returns Some of a tuple including the SingleOntologyNodeGrounding and the
  // Range of the match in the splitText so that we can tell how much of it was used.  No match results in None.
  def exactMatchForPreds(splitText: Array[String], embeddings: Seq[ConceptEmbedding]): Option[(SingleOntologyNodeGrounding, Range)] = {
    // This looks for exact string overlap only!
    // This tuple is designed so that Seq.min gets the intended result, the one with the min negLength
    // (or max length) and in case of ties, the min position in the sentence, so the leftmost match.
    // The embedding.namer should not be required to break ties.  It goes along for the ride.
    // For expediency, the word count is used for length rather than the letter count.
    val overlapTuples = embeddings.flatMap { embedding =>
      val canonicalWords = embedding.namer.canonicalWords

      if (canonicalWords.isEmpty)
        // Non-leaf nodes end with a / resulting in an empty canonicalWords which we don't want to match.
        None
      else if (splitText.length >= canonicalWords.length) {
        // Text contains node name.
        val index = splitText.indexOfSlice(canonicalWords)
        if (index < 0) None
        // Part or maybe all of the split text was matched, indicated by 1, favored.
        else Some(-canonicalWords.length, index, 1, embedding.namer)
      }
      else {
        // Node name contains the text
        val index = canonicalWords.indexOfSlice(splitText)
        if (index < 0) None
        // The entirety of splitText was matched, indicated by 2, disfavored.
        else Some(-splitText.length, 0, 2, embedding.namer)
      }
    }
    val result = Collection
        .minOption(overlapTuples)
        .map { overlapTuple =>
          val singleOntologyNodeGrounding = SingleOntologyNodeGrounding(overlapTuple._4, 1.0f)
          val range = Range(overlapTuple._2, overlapTuple._2 - overlapTuple._1) // - because it is -length

          (singleOntologyNodeGrounding, range)
        }

    result
  }

  def groundPatternsThenEmbeddings(text: String, splitText: Array[String], patterns: Seq[ConceptPatterns], examples: Seq[ConceptExamples], embeddings: Seq[ConceptEmbedding]): MultipleOntologyGrounding = {
    val lowerText = text.toLowerCase
    val exactMatches = embeddings.filter(_.namer.canonicalName == lowerText)
    if (exactMatches.nonEmpty)
      exactMatches.map(exactMatch => SingleOntologyNodeGrounding(exactMatch.namer, 1.0f))
    else {
      val matchedPatterns = nodesPatternMatched(text, patterns)
      if (matchedPatterns.nonEmpty)
        matchedPatterns
      else {
        // Otherwise, back-off to the w2v-based approach
        // TODO: The line below only uses the positive embeddings, not the negative ones.
        //  Should something be done there or here to take them into account.
        val matchedExamples = nodesExampleMatched(text, examples)
        val matchedEmbeddings = wordToVec.calculateSimilarities(splitText, embeddings)
        val embeddingGroundings = // This is a Seq rather than a Map.
            for ((namer, embeddingScore) <- matchedEmbeddings)
            yield {
              val exampleScore = matchedExamples(namer)
              // val comboScore = embeddingScore
              // val comboScore = embeddingScore + (1 / (exampleScore + 1)) // Becky's simple version
              val comboScore = embeddingScore + 1 / (log(exampleScore + 1) + 1)
              // val comboScore = pow(embeddingScore.toDouble, exampleScore.toDouble)
              SingleOntologyNodeGrounding(namer, comboScore.toFloat)
            }

        embeddingGroundings// ++ returnedExactMatches ++ matchedPatterns
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
  def filterAndSlice(fullGrounding: MultipleOntologyGrounding, topNOpt: Option[Int] = None, thresholdOpt: Option[Float] = None): MultipleOntologyGrounding = {
    val filtered = thresholdOpt.map { threshold =>
      fullGrounding.filter { i: IndividualGrounding => i.score >= threshold }
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
