package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, DomainOntology, EidosWordToVec, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.slf4j.{Logger, LoggerFactory}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import SRLCompositionalGrounder._

import scala.annotation.tailrec

case class GroundedToken(index: Int, grounding: OntologyGrounding)
case class PredicatePackage(predicate: GroundedToken, agent: Seq[GroundedToken] = Nil, theme: Seq[GroundedToken] = Nil, other: Seq[GroundedToken] = Nil)

class SRLCompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer)
  extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  lazy val proc = new FastNLPProcessorWithSemanticRoles

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

  // primarily used for passing in the canonical name parts
  override def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    SRLCompositionalGrounder.logger.info("The SRLCompositionalGrounder isn't designed to be used with canonical name parts only.")
    ???
  }

  override def groundText(text: String): OntologyGrounding = {
    val doc = proc.annotate(text)
    val groundings = for {
      s <- doc.sentences
      // FIXME -- the empty sequence here is a placeholder for increase/decrease triggers
      ontologyGrounding <- groundSentenceSpan(s, 0, s.words.length, Seq())
      singleGrounding <- ontologyGrounding.grounding
    } yield singleGrounding

    val groundingResult = newOntologyGrounding(groundings.sortBy(- _.score))
    groundingResult
  }

  def groundSentenceSpan(s: Sentence, start: Int, end: Int, exclude: Seq[Int]): Seq[OntologyGrounding] = {
    val tokenInterval = Interval(start, end)
    groundSentenceSpan(s, tokenInterval, exclude)
  }
  def groundSentenceSpan(s: Sentence, tokenInterval: Interval, exclude: Seq[Int]): Seq[OntologyGrounding] = {
    //  3) for each Concept (cause/effect), look for all predicates --> these are the processes
    val conceptPredicates = groundPredicates(s, tokenInterval, exclude)

    //  9) ask Keith to pretty please make a way to output it in the json
    ???
  }

  override def groundEidosMention(mention: EidosMention, topN: Option[Int] = None, threshold: Option[Float] = None): Seq[OntologyGrounding] = {
    // Do nothing to non-groundableType mentions
    if (!EidosOntologyGrounder.groundableType(mention))
      Seq(newOntologyGrounding())
    // or else ground them.
    else {
      val attachmentTriggers = findAttachmentTriggers(mention.odinMention)
      groundSentenceSpan(mention.odinMention.sentenceObj, mention.odinMention.start, mention.odinMention.end, attachmentTriggers)
    }
  }



  def groundPredicates(sentence: Sentence, tokenInterval: Interval, exclude: Seq[Int]): Seq[PredicatePackage] = {
    val srls = sentence.semanticRoles.get
    val predicates = srls.roots.toSeq
      // keep only predicates that are within the mention
      .filter(tokenInterval contains _)
      // remove the predicates which correspond to our increase/decrease/quantifiers
      .filterNot(exclude contains _)
      // start with those closest to the syntactic root of the sentence to begin with "higher level" predicates
      .sortBy(minGraphDistanceToSyntacticRoot(_, sentence.dependencies.get))

    packagePredicates(predicates, Seq(), Set(), srls, predicates.toSet)
  }



  @tailrec
  private def packagePredicates(remaining: Seq[Int], results: Seq[PredicatePackage], seen: Set[Int], srls: DirectedGraph[String], predicates: Set[Int]): Seq[PredicatePackage] = {
    remaining match {
      case Seq() => results
      case Seq(curr) => results ++ Seq(mkPredicatePackage(curr, srls, predicates))
      case curr :: rest =>
        if (seen contains curr) {
          // we've been here before
          packagePredicates(rest, results, seen, srls, predicates)
        } else if (srls.roots contains curr) {
          // Otherwise, it's new and it's a predicate
          val packaged = mkPredicatePackage(curr, srls, predicates)
          packagePredicates(rest, results ++ Seq(packaged), seen ++ Set(curr), srls, predicates)
        }
        else {
          ???
          // I don't think this should happen
          // It's not a predicate, meaning it has already been included in some other predicate's package
          // packagePredicate(rest, results, seen ++ Set(curr), srls, predicates)
        }
    }
  }

  // Ground the predicate and also ground each of the relevant arguments
  // Return a PredicatePackage object
  private def mkPredicatePackage(predicate: Int, srls: DirectedGraph[String], validPredicates: Set[Int]): PredicatePackage = {
    // Otherwise, it's new.  If it's a predicate:
    val groundedPredicate = groundToken(predicate, validPredicates)
    // get the arguments
    val agents = groundArguments(predicate, AGENT_ROLE, srls, validPredicates)
    val themes = groundArguments(predicate, THEME_ROLE, srls, validPredicates)
    val others = groundArguments(predicate, OTHER_ROLE, srls, validPredicates)
    PredicatePackage(groundedPredicate, agents, themes, others)
  }

  // Find all arguments from a given predicate of a certain role (e.g., A1, A0, etc) and ground them
  private def groundArguments(predicate: Int, role: String, srls: DirectedGraph[String], validPredicates: Set[Int]): Seq[GroundedToken] = {
    srls.outgoingEdges(predicate)
      .filter(_._2 == role)
      .map(tok => groundToken(tok._1, validPredicates))
  }

  // Ground a single token in isolation
  //  4) if an arg is itself a predicate => it's a process
  //  6) else if it matches a property regex => it's a property
  //  7) else it's a concept
  //  8) assemble the groundings for above steps into the data structure
  private def groundToken(token: Int, validPredicates: Set[Int]): GroundedToken = {
    if (validPredicates contains token) {
      GroundedToken(token, groundProcess())
    } else if (isProperty()) {
      GroundedToken(token, groundProperty())
    } else {
      GroundedToken(token, groundConcept())
    }
  }

  private def isProperty(): Boolean = ???

  // Find the shortest distance (in the syntax graph) between a given token and any of the roots
  private def minGraphDistanceToSyntacticRoot(token: Int, deps: DirectedGraph[String]): Int = {
    // Get the sentence roots -- there can be more than one
    val roots = deps.roots
    // for each, check the shortest path from that root to the given token
    val pathLengths = roots.map(root => deps.shortestPath(root, token, ignoreDirection = true).length)
    // select the shortest to be the distance from the token to any of the roots
    pathLengths.min
  }
  private def findAttachmentTriggers(mention: Mention): Seq[Int] = ???
  private def isModifierTrigger(token: Int, mention: Mention): Boolean = ???

  private  def groundProcess() = groundBranch(PROCESS)
  private def groundProperty() = groundBranch(PROPERTY)
  private def groundConcept() = groundBranch(CONCEPT)

  private def groundBranch(branch: String) = ???

}

object SRLCompositionalGrounder{

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
  // Semantic Roles
  val AGENT_ROLE = "A0"
  val THEME_ROLE = "A1"
  val OTHER_ROLE = "AX"
  val TIME_ROLE = "AM-TMP"
  val LOC_ROLE = "AM-LOC" // FIXME: may need offline processing to make happen
  // Compositional Ontology Branches
  val PROCESS = "process"
  val CONCEPT = "concept"
  val PROPERTY = "property"
}
