package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer
import org.clulab.struct.Interval
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

// TODO: Zupon
class CompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  protected lazy val conceptEmbedddingsSeq: Seq[(String, Seq[ConceptEmbedding])] = {

    def getBranch(branch: String): (String, Seq[ConceptEmbedding]) =
        branch -> conceptEmbeddings.filter { _.namer.branch.contains(branch) }

    Seq(
      getBranch("process"),
      getBranch("property"),
      getBranch("phenomenon")
    ).filter { case (_, conceptEmbeddings) => conceptEmbeddings.nonEmpty }
  }

  override def groundOntology(mention: EidosMention): Seq[OntologyGrounding] = {
    val canonicalNameParts = canonicalizer.canonicalNameParts(mention)


    /** Get all dependencies within the original mention */

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

    /** Get syntactic head of mention */

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
    // text of syntactic head
    val headText = Array(mentionHead.head.text)

    /** Get modifiers of syntactic head */
    val modifiers = new ArrayBuffer[Mention]
    val allowedMods = List("compound", "nmod_of", "nmod_to", "nmod_for") // TODO: may need tuning

    val argumentIntervals = mention.odinMention.arguments.values.flatten.map(_.tokenInterval)
    for {
      tok <- mention.odinMention.tokenInterval
      if !argumentIntervals.exists(_.contains(tok))
      in <- incoming.lift(tok)
      (_, label) <- in
      if allowedMods.contains(label)
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
    // text of modifiers
    var modText = Array[String]()
    for (mod <- modifiers) {
      modText += mod.text
    }

    // combine head with modifiers, head first
    val allMentions = mentionHead ++ modifiers
    val allText = headText ++ modText


    // FIXME: I'm sure there's a WAY better way to do this;
    //  done to compare words with nodes in branches of ontology
    // get Namers for processes
    var processNamers = Array[Namer]()
    for (embedding <- conceptEmbedddingsSeq(0)._2) { // should be "process" branch
      val embeddingNamer = embedding.namer
      processNamers = processNamers :+ embeddingNamer
    }
    // get Namers for properties
    var propertyNamers = Array[Namer]()
    for (embedding <- conceptEmbedddingsSeq(1)._2) { // should be "property" branch
      val embeddingNamer = embedding.namer
      propertyNamers = propertyNamers :+ embeddingNamer
    }
    // get Namers for phenomena
    var phenomenonNamers = Array[Namer]()
    for (embedding <- conceptEmbedddingsSeq(2)._2) { // should be "phenomenon" branch
      val embeddingNamer = embedding.namer
      phenomenonNamers = phenomenonNamers :+ embeddingNamer
    }

    // keep a placeholder for each component
    var propertyGrounding: MultipleOntologyGrounding = Seq.empty
    var processGrounding: MultipleOntologyGrounding = Seq.empty
    var phenomGrounding: MultipleOntologyGrounding = Seq.empty

    // hyperparameter for how many groundings to take
    // should probably be inherited from somewhere else, rather than defined here
    val k = 5

    // for each word in the mention
    for (mention <- allMentions) {
      // Sieve-based approach
      if (EidosOntologyGrounder.groundableType(EidosMention.asEidosMentions(Seq(mention)).head)) {
        // First check to see if the text matches a regex from the ontology, if so, that is a very precise
        // grounding and we want to use it.
        val matchedPatterns = nodesPatternMatched(mention.text, conceptPatterns)
        val matchedPatternNamer = matchedPatterns.head._1
        if (matchedPatterns.nonEmpty && propertyNamers.contains(matchedPatternNamer)) {
          val propertyGround =
            propertyGrounding.take(propertyGrounding.length) ++
            OntologyGrounding(matchedPatterns, Some("property")).take(matchedPatterns.length)
          propertyGrounding = propertyGround.sortBy(-_._2) // sort by confidence score high to low
        }
        // Otherwise, back-off to the w2v-based approach
        else {
          var w2vNamers = Seq[Namer]()
          for (embedding <- w2v.calculateSimilarities(headText, conceptEmbeddings)) {
            val w2vNamer = embedding._1
            w2vNamers = w2vNamers :+ w2vNamer
          }
          if (processNamers.contains(w2vNamers.head)) {
            // if it's not a "property", try to ground to "process" first
            // fixme: probably won't match exactly?
            val processGround =
              processGrounding.take(processGrounding.length) ++
              OntologyGrounding(w2v.calculateSimilarities(headText, conceptEmbeddings), Some("process")).take(k)
            processGrounding = processGround.sortBy(-_._2) // sort by confidence score high to low
          }
          else {
            // otherwise ground to "phenomenon"
            val phenomGround =
              phenomGrounding.take(phenomGrounding.length) ++
              OntologyGrounding(w2v.calculateSimilarities(headText, conceptEmbeddings), Some("phenomenon")).take(k)
            phenomGrounding = phenomGround.sortBy(-_._2) // sort by confidence score high to low
          }
        }
      }
      else {
//        val returnedGroundings = Seq(OntologyGrounding())
      }
    }

    val returnedGroundings = Seq(
      OntologyGrounding(propertyGrounding, Some("property")),
      OntologyGrounding(processGrounding, Some("process")),
      OntologyGrounding(phenomGrounding, Some("phenomenon")))



// todo: not sure what exactly this is returning for "branch"
    val returned = conceptEmbedddingsSeq.map { case (branch, conceptEmbeddings) =>
      OntologyGrounding(w2v.calculateSimilarities(canonicalNameParts, conceptEmbeddings), Some(branch))
    }
    returnedGroundings
  }
}

// TODO: Zupon
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

  val indicatorNamespaces: Set[String] = Set(WDI_NAMESPACE, FAO_NAMESPACE, MITRE12_NAMESPACE, WHO_NAMESPACE, ICASA_NAMESPACE)

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
