package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.odin.{ExtractorEngine, Mention, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.struct.Interval
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.OdinUtils
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

case class OntologyGrounding(version: Option[String], date: Option[ZonedDateTime], grounding: MultipleOntologyGrounding = Seq.empty, branch: Option[String] = None) {
  def nonEmpty: Boolean = grounding.nonEmpty
  def take(n: Int): MultipleOntologyGrounding = grounding.take(n)
  def headOption: Option[SingleOntologyGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_._1.name)
}

trait OntologyGrounder {
  def name: String
  def domainOntology: DomainOntology
  def groundOntology(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding]
  def groundStrings(strings: Array[String]): Seq[OntologyGrounding]
}

abstract class EidosOntologyGrounder(val name: String, val domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends OntologyGrounder {

  def newOntologyGrounding(grounding: OntologyAliases.MultipleOntologyGrounding = Seq.empty, branch: Option[String] = None): OntologyGrounding = {
    OntologyGrounding(domainOntology.version, domainOntology.date, grounding, branch)
  }

  // TODO: These may have to change depending on whether n corresponds to leaf or branch node.
  val conceptEmbeddings: Seq[ConceptEmbedding] =
    0.until(domainOntology.size).map { n =>
      ConceptEmbedding(domainOntology.getNamer(n), wordToVec.makeCompositeVector(domainOntology.getValues(n)))
    }

  val conceptPatterns: Seq[ConceptPatterns] =
    0.until(domainOntology.size).map { n =>
      ConceptPatterns(domainOntology.getNamer(n), domainOntology.getPatterns(n))
    }

  // For API to reground strings
  def groundOntology(isGroundableType: Boolean, mentionText: String, canonicalNameParts: Array[String]): OntologyGrounding = {
    // Sieve-based approach
    if (isGroundableType) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mentionText, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        newOntologyGrounding(matchedPatterns)
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        newOntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings))
      }
    }
    else
      newOntologyGrounding()
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
      newOntologyGrounding(matchedPatterns)
    }
    // Otherwise, back-off to the w2v-based approach
    else {
      newOntologyGrounding(wordToVec.calculateSimilarities(text.split(" +"), conceptEmbeddings))
    }
  }
}

class FlatOntologyGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {
  // TODO Move some stuff from above down here if it doesn't apply to other grounders.

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(wordToVec.calculateSimilarities(strings, conceptEmbeddings)))
  }

  def groundOntology(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    // Sieve-based approach
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the ontology, if so, that is a very precise
      // grounding and we want to use it.
      val matchedPatterns = nodesPatternMatched(mention.odinMention.text, conceptPatterns)
      if (matchedPatterns.nonEmpty) {
        Seq(newOntologyGrounding(matchedPatterns))
      }
      // Otherwise, back-off to the w2v-based approach
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)
        groundStrings(canonicalNameParts)
      }
    }
    else
      Seq(newOntologyGrounding())
  }
}

class InterventionSieveGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
  extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {

  // FIXME: gross hack
  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
   ???
  }

  def inBranch(s: String, branches: Seq[ConceptEmbedding]): Boolean =
    branches.exists(_.namer.name == s)

  protected lazy val conceptEmbeddingsSeq: Map[String, Seq[ConceptEmbedding]] = {
    val (interventionNodes, rest) = conceptEmbeddings.partition(_.namer.name.contains("causal_factor/interventions"))
    Map(
      InterventionSieveGrounder.INTERVENTION -> interventionNodes,
      InterventionSieveGrounder.REST -> rest
    )
  }

  protected lazy val conceptPatternsSeq: Map[String, Seq[ConceptPatterns]] = {
    val (interventionNodes, rest) = conceptPatterns.partition(_.namer.name.contains("causal_factor/interventions"))
    Map(
      InterventionSieveGrounder.INTERVENTION -> interventionNodes,
      InterventionSieveGrounder.REST -> rest
    )
  }



  def groundOntology(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the main part of the ontology,
      // if so, that is a very precise grounding and we want to use it.
      val matchedPatternsMain = nodesPatternMatched(mention.odinMention.text, conceptPatternsSeq(InterventionSieveGrounder.REST))
      if (matchedPatternsMain.nonEmpty) {
        Seq(newOntologyGrounding(matchedPatternsMain))
      }
      // Otherwise, back-off to the w2v-based approach for main branch and a sieve for interventions
      else {
        val groundings = new ArrayBuffer[OntologyGrounding]
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

        // Main Portion of the ontology
        val mainConceptEmbeddings = conceptEmbeddingsSeq(InterventionSieveGrounder.REST)
        val mainGroundings = Seq(newOntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, mainConceptEmbeddings)))
        groundings.appendAll(mainGroundings)

        // Intervention Branch
        // Only allow grounding to these nodes if the patterns match
        val possibleIntervention = nodePatternsMatch(mention.odinMention.text, Some(InterventionSieveGrounder.regexes))
        if (possibleIntervention) {
          val matchedPatternsInterventions = nodesPatternMatched(mention.odinMention.text, conceptPatternsSeq(InterventionSieveGrounder.INTERVENTION))
          // If you match a node pattern, give it a score of 1.0
          if (matchedPatternsInterventions.nonEmpty) {
            groundings.append(newOntologyGrounding(matchedPatternsInterventions))
            // todo: don't use 1
            // use the w2v score to allow to compete more fairly
            // soften the regexes in the ontology...
            // global patterns (any node)
            // specific pattern, match local
            // add some the regexes below to the ontology...
            // pair the interventions embeddings with the patterns
            // parallel, sanity check assert
            // rerun, check
          }
//          else {
////            val interventionConceptEmbeddings = conceptEmbeddingsSeq(InterventionSieveGrounder.INTERVENTION)
////            val interventionGroundings = Seq(newOntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, interventionConceptEmbeddings)))
////            groundings.appendAll(interventionGroundings)
//          }
        }
        // fixme: rank properly!
        groundings
       }
    }
    else
      Seq(newOntologyGrounding())

  }

}

object InterventionSieveGrounder {
  val INTERVENTION = "interventions"
  val REST = "rest"

  val branches: Seq[String] = Seq(INTERVENTION, REST)

  val regexes = Array(
    // humanitarian assistance
    "(?i)(intervention)".r,
    """(?i)(humanitarian)(\s|\w)+(aid|assistance)""".r,
    // provision of goods and services
    """(?i)((provision\s+of)|(distribution\s+of)|providing|provided?|distributed?)\s+(?!.*(rain|rainfall))""".r,
    """(?i)(\w)+(?<!rain|rainfall)\s+(provision|distribution|assistance)""".r,
    // infrastructure
    """(((rehabilitation|restoration|construction|repair)\s+of)|(build|restoring|constructing|repairing))""".r,
    """(disaster\s+relief|field\s+hospital|classrooms)""".r,
    """(improvement\s+of|improved)(\s|\w)+(school|infrastructure|education|facilit)""".r,
    """(temporary|relief|deployment)(\s|\w)+(camp|building|facilit|shelter|settlement)""".r,
    """(borehole\s+drilling)""".r,
    // peacekeeping and security
    """(peacekeeping|peacekeepers|peace\s+(talk|treaty)|political\s+mediation)""".r,
    """(conflict)(\s|\w)+(resolution|mediation|intervention)""".r,
    """(formal\s+mediation)""".r,
    """(ceasefire|cease\s+fire)""".r,
    """(disarm|demobiliz|reintegrat)""".r,
    // institutional support
    """((engag|interact|cooperation)(\s|\w)+with|support)(authorit|institution)""".r,
    """(support|develop|strengthen)(\s|\w)+(capacity|framework)""".r,
    """(integrat)(\s|\w)+into(\s|\w)+(policy|policies|program)""".r,
    """(capacity\s+building)""".r,
    """(public\s+sector\s+support)""".r,
    """(invest)(\s|\w)+(in)""".r,
  )
}

class CompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  def inBranch(s: String, branches: Seq[ConceptEmbedding]): Boolean =
      branches.exists(_.namer.name == s)

  protected lazy val conceptEmbeddingsSeq: Map[String, Seq[ConceptEmbedding]] =
      CompositionalGrounder.branches.map { branch =>
        (branch, conceptEmbeddings.filter { _.namer.branch.contains(branch) })
      }.toMap

  protected lazy val conceptPatternsSeq: Map[String, Seq[ConceptPatterns]] =
      CompositionalGrounder.branches.map { branch =>
        (branch, conceptPatterns.filter { _.namer.branch.contains(branch) })
      }.toMap

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    var property = ArrayBuffer(): Seq[(Namer,Float)]
    for (string <- strings) {
      val matchedPatterns = nodesPatternMatched(string, conceptPatternsSeq(CompositionalGrounder.PROPERTY))
      if (matchedPatterns.nonEmpty) {
        property = property ++ matchedPatterns
      }
    }
    val process = newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddingsSeq(CompositionalGrounder.PROCESS)), Some(CompositionalGrounder.PROCESS))
    val concept = newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT)), Some(CompositionalGrounder.CONCEPT))

    Seq(newOntologyGrounding(property, Some(CompositionalGrounder.PROPERTY)), process, concept)
  }

  override def groundOntology(mention: EidosMention, topN: Option[Int] = None, threshold: Option[Float] = None): Seq[OntologyGrounding] = {
    // Do nothing to non-groundableType mentions
    if (!EidosOntologyGrounder.groundableType(mention))
      Seq(newOntologyGrounding())
    // or else ground them.
    else {
      // Get the syntactic head of the mention.
      val syntacticHeadOpt = mention.odinMention.synHead
      // Make a new mention that's just the syntactic head of the original mention.
      val mentionHeadOpt = syntacticHeadOpt.map ( syntacticHead =>
        new TextBoundMention(
          Seq("Mention_head"),
          tokenInterval = Interval(syntacticHead),
          sentence = mention.odinMention.sentence,
          document = mention.odinMention.document,
          keep = mention.odinMention.keep,
          foundBy = mention.odinMention.foundBy
        )
      )
      val headTextOpt = mentionHeadOpt.map(_.text)
      val modifierMentions = headTextOpt.map { headText =>
        getModifierMentions(headText, mention.odinMention)
      }.getOrElse(Seq.empty)
      val allMentions = mentionHeadOpt.toSeq ++ modifierMentions
      // Get all groundings for each branch.
      val allSimiliarities = {
        val allMentionTexts = allMentions.map { mention => mention.words.toArray }

        Map(
          CompositionalGrounder.PROPERTY ->
              allMentions.flatMap(m => nodesPatternMatched(m.text, conceptPatternsSeq(CompositionalGrounder.PROPERTY))),
          CompositionalGrounder.PROCESS ->
              allMentionTexts.flatMap(mentionTexts => w2v.calculateSimilarities(mentionTexts, conceptEmbeddingsSeq(CompositionalGrounder.PROCESS))),
          CompositionalGrounder.CONCEPT ->
              allMentionTexts.flatMap(mentionTexts => w2v.calculateSimilarities(mentionTexts, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT)))
        )
      }
      val effectiveThreshold = threshold.getOrElse(CompositionalGrounder.defaultThreshold)
      val effectiveTopN = topN.getOrElse(CompositionalGrounder.defaultGroundTopN)
      val goodGroundings = allSimiliarities.map { case(name, similarities) =>
        val goodSimilarities = similarities
            .filter(_._2 >= effectiveThreshold) // Filter these before sorting!
            .sortBy(-_._2)
            .take(effectiveTopN)

        newOntologyGrounding(goodSimilarities, Some(name))
      }.toSeq

      goodGroundings
    }
  }

  def getModifierMentions(synHeadWord: String, mention: Mention): Seq[Mention] = {
    val doc = Document(Array(mention.sentenceObj))

    // FIXME: do we need VPs too?
    // FIXME: issue with  multiple copies of the same head word, e.g. "price of oil increase price of transportation"
    val rule = CompositionalGrounder.ruleTemplates.replace(CompositionalGrounder.SYN_HEAD_WORD,
        OdinUtils.escapeExactStringMatcher(synHeadWord))
    val engine = ExtractorEngine(rule)
    val results = engine.extractFrom(doc)
    val mods = results.filter(_ matches "InternalModifier")
    val modifierArgs = mods.flatMap(m => m.arguments("modifier")).distinct

    modifierArgs
  }
}

object CompositionalGrounder {
  val PROCESS = "process"
  val PROPERTY = "property"
  val CONCEPT =  "concept"

  val branches: Seq[String] = Seq(PROCESS, PROPERTY, CONCEPT)

  // FIXME: these should connect to a config probably...?
  val defaultThreshold: Float = 0.5f
  val defaultGroundTopN = 5

  val SYN_HEAD_WORD = "$synHeadWord"

  // See documentation at https://stackoverflow.com/questions/3790454/how-do-i-break-a-string-over-multiple-lines.
  // Some values need to be entered into the yaml structure at the right place.
  // Do not "s" a yaml string (in general), because then the yaml may not be escaped properly.
  // In this case, all substitutions are after a | which,
  // "allow[s] characters such as \ and " without escaping, and add a new line (\n) to the end of your string".
  // These are exactly the characters that might be inserted in OdinUtils.escapeExactStringMatcher.
  val ruleTemplates: String =
      s"""
        | - name: AllWords
        |   label: PotentialModifier
        |   priority: 1
        |   type: token
        |   pattern: |
        |      [chunk=/NP$$/ & !word=$SYN_HEAD_WORD & !tag=/DT|JJ|CC/]
        |
        | - name: SegmentConcept
        |   label: InternalModifier
        |   priority: 2
        |   pattern: |
        |      trigger = $SYN_HEAD_WORD
        |      modifier: PotentialModifier+ = >/^(compound|nmod_of|nmod_to|nmod_for|nmod_such_as)/{0,2} >/amod|compound/?
          """.stripMargin
}

// TODO: Zupon
class InterventionGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
    // TODO This might extend something else
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    Seq(newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddings), Some("intervention")))
  }

  def groundOntology(mention: EidosMention, topN: Option[Int] = Option(5), threshold: Option[Float] = Option(0.5f)): Seq[OntologyGrounding] = {
    val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

    groundStrings(canonicalNameParts)
  }
}

object EidosOntologyGrounder {
            val                 GROUNDABLE = "Entity"
  protected val               WM_NAMESPACE = "wm" // This one isn't in-house, but for completeness...
  protected val WM_COMPOSITIONAL_NAMESPACE = "wm_compositional"
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
  protected val   MAAS_NAMES = Set("MaaS-model", "MaaS-parameter", "MaaS-variable")
  protected val   WM_FLAT_NAMESPACE = "wm_flat"

  val PRIMARY_NAMESPACE: String = WM_FLATTENED_NAMESPACE // Assign the primary namespace here, publically.

  val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE) ++ MAAS_NAMES

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def groundableType(mention: EidosMention): Boolean = mention.odinMention.matches(GROUNDABLE)

  def apply(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer): EidosOntologyGrounder = {
    new FlatOntologyGrounder(name, domainOntology, wordToVec, canonicalizer)
  }

  def mkGrounder(ontologyName: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer): OntologyGrounder = {
    ontologyName match {
      case WM_COMPOSITIONAL_NAMESPACE => new CompositionalGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case INTERVENTIONS_NAMESPACE => new InterventionGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case WM_FLAT_NAMESPACE => new InterventionSieveGrounder(ontologyName, domainOntology, w2v, canonicalizer)
      case _ => EidosOntologyGrounder(ontologyName, domainOntology, w2v, canonicalizer)
    }
  }
}
