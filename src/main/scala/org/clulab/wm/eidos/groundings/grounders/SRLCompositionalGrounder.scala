package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.attachments.{ContextAttachment, TriggeredAttachment}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, DomainOntology, EidosWordToVec, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.slf4j.{Logger, LoggerFactory}

import SRLCompositionalGrounder._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding)
case class PredicatePackage(predicate: GroundedSpan, agent: Seq[GroundedSpan] = Nil, theme: Seq[GroundedSpan] = Nil, other: Seq[GroundedSpan] = Nil)

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
      // TODO (at some point) -- the empty sequence here is a placeholder for increase/decrease triggers
      //  Currently we don't have "access" to those here, but that could be changed
      ontologyGrounding <- groundSentenceSpan(s, 0, s.words.length, Set())
      singleGrounding <- ontologyGrounding.grounding
    } yield singleGrounding

    val groundingResult = newOntologyGrounding(groundings.sortBy(- _.score))
    groundingResult
  }

  override def groundEidosMention(mention: EidosMention, topN: Option[Int] = None, threshold: Option[Float] = None): Seq[OntologyGrounding] = {
    // Do nothing to non-groundableType mentions
    if (!EidosOntologyGrounder.groundableType(mention))
      Seq(newOntologyGrounding())
    // or else ground them.
    else {
      groundSentenceSpan(mention.odinMention.sentenceObj, mention.odinMention.start, mention.odinMention.end, attachmentStrings(mention.odinMention))
    }
  }

  def groundSentenceSpan(s: Sentence, start: Int, end: Int, exclude: Set[String]): Seq[OntologyGrounding] = {
    val tokenInterval = Interval(start, end)
    groundSentenceSpan(s, tokenInterval, exclude)
  }

  def groundSentenceSpan(s: Sentence, tokenInterval: Interval, exclude: Set[String]): Seq[OntologyGrounding] = {
    val conceptPredicates = groundPredicates(s, tokenInterval, exclude)
    // TODO: Convert these into OntologyGroundings
    ???
  }

  def groundPredicates(sentence: Sentence, tokenInterval: Interval, exclude: Set[String]): Seq[PredicatePackage] = {
    val sentenceHelper = SentenceHelper(sentence, tokenInterval, exclude)
    packagePredicates(sentenceHelper.validPredicates, Seq(), Set(), sentenceHelper)
  }


  @tailrec
  private def packagePredicates(remaining: Seq[Int], results: Seq[PredicatePackage], seen: Set[Int], s: SentenceHelper): Seq[PredicatePackage] = {
    remaining match {
      case Seq() => results
      case Seq(curr) => results ++ Seq(mkPredicatePackage(curr, s))
      case curr :: rest =>
        if (seen contains curr) {
          // we've been here before
          packagePredicates(rest, results, seen, s)
        } else if (s.srls.roots contains curr) {
          // Otherwise, it's new and it's a predicate
          // TODO: this currently does not "nest" the predicates, if we decide we want to, we'll need to adjust it
          //  if on the other hand, we want only to output the 4-tuples, then it shouldn't matter
          val packaged = mkPredicatePackage(curr, s)
          packagePredicates(rest, results ++ Seq(packaged), seen ++ Set(curr), s)
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
  private def mkPredicatePackage(predicate: Int, s: SentenceHelper): PredicatePackage = {
    // Otherwise, it's new.  If it's a predicate:
    val groundedPredicate = groundChunk(predicate, s)
    // get the arguments
    val agents = groundArguments(predicate, AGENT_ROLE, s)
    val themes = groundArguments(predicate, THEME_ROLE, s)
    val others = groundArguments(predicate, OTHER_ROLE, s)
    PredicatePackage(groundedPredicate, agents, themes, others)
  }

  // Find all arguments from a given predicate of a certain role (e.g., A1, A0, etc) and ground them
  private def groundArguments(predicate: Int, role: String, s:SentenceHelper): Seq[GroundedSpan] = {
    s.srls.outgoingEdges(predicate)
      .filter(_._2 == role)
      .map(tok => groundChunk(tok._1, s))
  }

  // Ground the chunk that the token is in, but in isolation from the rest of the sentence
  //  a) if an arg is itself a predicate => it's a process
  //  b) else if it matches a property regex => it's a property
  //  c) else it's a concept
  private def groundChunk(token: Int, s: SentenceHelper): GroundedSpan = {
    val chunkSpan = s.chunkIntervals.collect{ case c if c.contains(token) => c} match {
      case Seq() =>
        logger.warn(s"Token $token is not in a chunk.  chunks: ${s.chunks.mkString(", ")}")
        Interval(token, token + 1)  // if empty, backoff
      case Seq(chunk) => chunk      // one found, yay! We'll use it
      case chunks => throw new RuntimeException(s"Chunks have overlapped, there is a problem.  \n\ttoken: $token\n\tchunks: ${chunks.mkString(", ")}")
    }
    if (s.validPredicates contains token) {
      GroundedSpan(chunkSpan, groundProcess(chunkSpan, s))
    } else if (isProperty(chunkSpan, s)) {
      GroundedSpan(chunkSpan, groundProperty(chunkSpan, s))
    } else {
      GroundedSpan(chunkSpan, groundConcept(chunkSpan, s))
    }
  }


  private def isProperty(span: Interval, s: SentenceHelper): Boolean = {
    val spanText = s.wordsSliceString(span)
    nodesPatternMatched(spanText, conceptPatternsSeq(PROPERTY)).nonEmpty
  }

  // Find the shortest distance (in the syntax graph) between a given token and any of the roots
  private def minGraphDistanceToSyntacticRoot(token: Int, deps: DirectedGraph[String]): Int = {
    // Get the sentence roots -- there can be more than one
    val roots = deps.roots
    // for each, check the shortest path from that root to the given token
    val pathLengths = roots.map(root => deps.shortestPath(root, token, ignoreDirection = true).length)
    // select the shortest to be the distance from the token to any of the roots
    pathLengths.min
  }

  // Get the triggers, quantifiers, and context phrases of the attachments
  private def attachmentStrings(mention: Mention): Set[String] = {
    mention.attachments.flatMap { a =>
      a match {
        case t: TriggeredAttachment => Seq(t.trigger) ++ t.quantifiers.getOrElse(Seq())
        case c: ContextAttachment => Seq(c.text)
        case _ => ???
      }
    }
  }

  private  def groundProcess(span: Interval, s: SentenceHelper): OntologyGrounding = groundBranch(PROCESS, span, s)
  private def groundProperty(span: Interval, s: SentenceHelper): OntologyGrounding = groundBranch(PROPERTY, span, s)
  private def groundConcept(span: Interval, s: SentenceHelper): OntologyGrounding = groundBranch(CONCEPT, span, s)

  private def groundBranch(branch: String, span: Interval, s: SentenceHelper): OntologyGrounding = {
    groundPatternsThenEmbeddings(s.wordsSlice(span), conceptPatternsSeq(branch), conceptEmbeddingsSeq(branch))
  }


  // Helper class to hold a bunch of things that are needed throughout the process for grounding
  case class SentenceHelper(sentence: Sentence, tokenInterval: Interval, exclude: Set[String]) {
    val chunks: Array[String] = sentence.chunks.get
    val chunkIntervals: Seq[Interval] = chunkSpans
    val words: Array[String] = sentence.words
    val srls: DirectedGraph[String] = sentence.semanticRoles.get
    // The roots of the SRL graph that are within the concept being grounded and aren't part of
    // an something we're ignoring (e.g., increase/decrease/quantification)
    val validPredicates: Seq[Int] = {
      srls.roots.toSeq
      // keep only predicates that are within the mention
      .filter(tokenInterval contains _)
      // remove the predicates which correspond to our increase/decrease/quantifiers
      .filterNot(exclude contains words(_))
      // start with those closest to the syntactic root of the sentence to begin with "higher level" predicates
      .sortBy(minGraphDistanceToSyntacticRoot(_, sentence.dependencies.get))
    }

    // Make a Seq of the Intervals corresponding to the syntactic chunks in the sentence (inclusive, exclusive).
    def chunkSpans: Seq[Interval] = {
      val chunkIntervals = new ArrayBuffer[Interval]
      var currStart = -1
      var currEnd = -1
      val chunks = sentence.chunks.get
      val numTokens = chunks.length
      for ((t, i) <- chunks.zipWithIndex) {
        if (t.startsWith("B")) {
          // New chunk has started, package the previous
          if (currStart != -1 && currEnd != -1) {
            chunkIntervals.append(Interval(currStart, currEnd))
          }
          currStart = i
          currEnd = -1
        } else if (t.startsWith("I")) {
          // if this isn't the last thing in the sentence
          if (i + 1 < numTokens) {
            if (chunks(i + 1) != t) {
              // the next chunk is different, so this is the end
              currEnd = i + 1
            }
          }
        } else {
          // chunk starts with "O", if this is the first one, mark it as the end
          if (currEnd == -1) currEnd = i
        }
      }
      // Handle any remaining chunks that didn't get added yet
      if (currStart != -1 && currEnd != -1) {
        chunkIntervals.append(Interval(currStart, currEnd))
      }
      chunkIntervals
    }

    def wordsSlice(span: Interval): Array[String] = words.slice(span.start, span.end)
    def wordsSliceString(span: Interval): String = wordsSlice(span).mkString(" ")
  }

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
