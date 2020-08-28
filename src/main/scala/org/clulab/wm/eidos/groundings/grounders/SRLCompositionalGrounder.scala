package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.attachments.{ContextAttachment, Property, TriggeredAttachment}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, DomainOntology, EidosWordToVec, OntologyGrounding, PredicateGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{Canonicalizer, GroundingUtils}
import org.slf4j.{Logger, LoggerFactory}
import SRLCompositionalGrounder._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding, isProperty: Boolean = false)
case class PredicateTuple(theme: OntologyGrounding, themeProperties: OntologyGrounding, themeProcess: OntologyGrounding, themeProcessProperties: OntologyGrounding) {

  val name: String = {
    if (theme.nonEmpty) {
      val sb = new ArrayBuffer[String]()
      sb.append(s"THEME: ${theme.headName.get}")
      if (themeProperties.nonEmpty) {
       sb.append(s" (Properties: ${themeProperties.take(5).map(_.name).mkString(", ")})")
      }
      if (themeProcess.nonEmpty) {
        sb.append(s"; THEME PROCESS: ${themeProcess.headName.get})")
      }
      if (themeProcessProperties.nonEmpty) {
        sb.append(s" (Properties: ${themeProcessProperties.take(5).map(_.name).mkString(", ")})")
      }
      sb.mkString("")
    } else {
      "Empty Compositional Grounding"
    }
  }
  val score: Float = {
    val themeScore = theme.grounding.headOption.map(_.score)
    val themeProcessScore = themeProcess.grounding.headOption.map(_.score)
    val allScores = themeScore ++ themeProcessScore
    if (allScores.isEmpty) 0.0f
    else GroundingUtils.noisyOr(allScores.toSeq)
  }
}

case class PredicatePackage(predicate: Seq[GroundedSpan], agent: Seq[GroundedSpan] = Nil, theme: Seq[GroundedSpan] = Nil, other: Seq[GroundedSpan] = Nil) {
  def toPredicateTuple(grounder: EidosOntologyGrounder): PredicateTuple = {
    def emptyGrounding: OntologyGrounding = grounder.newOntologyGrounding()
    def emptyTuple: PredicateTuple = PredicateTuple(emptyGrounding, emptyGrounding, emptyGrounding, emptyGrounding)
    def propsGrounding(props: Seq[GroundedSpan]): OntologyGrounding = {
      val allProps = props.flatMap(p => p.grounding.grounding)
      val branch = props.head.grounding.branch
      grounder.newOntologyGrounding(allProps, branch)
    }

    val (propertyThemes, otherThemes) = theme.partition(_.isProperty)
    // fixme: this isn't quite right -- what about the process properties??

    (predicate, otherThemes, propertyThemes) match {
      case (Seq(), Seq(), Seq())  => emptyTuple
      case (Seq(), Seq(), props)  =>
        println(s"There are properties, but no theme or process: $this")
        emptyTuple
      case (preds, Seq(), Seq())  => PredicateTuple( preds.head.grounding,  emptyGrounding,        emptyGrounding,       emptyGrounding)
      case (preds, Seq(), props)  => PredicateTuple( preds.head.grounding,  propsGrounding(props), emptyGrounding,       emptyGrounding)
      case (Seq(), themes, Seq()) => PredicateTuple( themes.head.grounding, emptyGrounding,        emptyGrounding,       emptyGrounding)
      case (Seq(), themes, props) => PredicateTuple( themes.head.grounding, propsGrounding(props), emptyGrounding,       emptyGrounding)
      case (preds, themes, Seq()) => PredicateTuple( themes.head.grounding, emptyGrounding,        preds.head.grounding, emptyGrounding)
      case (preds, themes, props) => PredicateTuple( themes.head.grounding, propsGrounding(props), preds.head.grounding, emptyGrounding)
      case _ => throw new RuntimeException(s"Unhandled situation in converting predicate to tuple: ${this.toString}")
    }
  }
}

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
    throw new RuntimeException("The SRLCompositionalGrounder isn't designed to be used with canonical name parts only.")
  }

  override def groundText(text: String): OntologyGrounding = {
    val doc = proc.annotate(text)
    val groundings = for {
      s <- doc.sentences
      // TODO (at some point) -- the empty sequence here is a placeholder for increase/decrease triggers
      //  Currently we don't have "access" to those here, but that could be changed
      //  Further, the Nones are for a topN and a threshold, which we don't have here
      ontologyGrounding <- groundSentenceSpan(s, 0, s.words.length, Set(), None, None)
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
      groundSentenceSpan(mention.odinMention.sentenceObj, mention.odinMention.start, mention.odinMention.end, attachmentStrings(mention.odinMention), topN, threshold)
    }
  }

  def groundSentenceSpan(s: Sentence, start: Int, end: Int, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding] = {
    val tokenInterval = Interval(start, end)
    groundSentenceSpan(s, tokenInterval, exclude, topN, threshold)
  }

  def groundSentenceSpan(s: Sentence, tokenInterval: Interval, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding] = {
    val conceptPredicates = groundPredicates(s, tokenInterval, exclude, topN, threshold)
    val predicateGroundings = conceptPredicates
      // Convert to the flattened tuple
      .map(_.toPredicateTuple(this))
      // Put in the format needed to export as a Grounding
      .map(PredicateGrounding)
    val filtered = filterAndSlice(predicateGroundings, topN, threshold)

    if (filtered.nonEmpty) {
      Seq(newOntologyGrounding(filtered))
    } else {
      val pseudoTheme = groundToBranches(Seq(CONCEPT), tokenInterval, s, topN, threshold)
      val predicateTuple = PredicateTuple(pseudoTheme, newOntologyGrounding(), newOntologyGrounding(), newOntologyGrounding())
      Seq(newOntologyGrounding(Seq(PredicateGrounding(predicateTuple)), Some(CONCEPT)))
    }
  }

  def groundPredicates(sentence: Sentence, tokenInterval: Interval, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[PredicatePackage] = {
    val sentenceHelper = SentenceHelper(sentence, tokenInterval, exclude)
    packagePredicates(sentenceHelper.validPredicates, Seq(), Set(), sentenceHelper, topN, threshold)
  }


  @tailrec
  private def packagePredicates(remaining: Seq[Int], results: Seq[PredicatePackage], seen: Set[Int], s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): Seq[PredicatePackage] = {
    remaining match {
      case Seq() => results
      case Seq(curr) => results ++ Seq(mkPredicatePackage(curr, s, topN, threshold))
      case Seq(curr, tail @ _*) =>
        if (seen contains curr) {
          // we've been here before
          packagePredicates(tail, results, seen, s, topN, threshold)
        } else if (s.srls.roots contains curr) {
          // Otherwise, it's new and it's a predicate
          // TODO: this currently does not "nest" the predicates, if we decide we want to, we'll need to adjust it
          //  if on the other hand, we want only to output the 4-tuples, then it shouldn't matter
          val packaged = mkPredicatePackage(curr, s, topN, threshold)
          println(s"packaged: $packaged")
          packagePredicates(tail, results ++ Seq(packaged), seen ++ Set(curr), s, topN, threshold)
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
  private def mkPredicatePackage(predicate: Int, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): PredicatePackage = {
    // Otherwise, it's new.  If it's a predicate:
    println("\n**************************************************")
    println(s"grounding the predicate: ${s.words(predicate)}")
    val groundedPredicate = groundChunk(predicate, s, topN, threshold)
    // get the arguments
    println(s"grounding the agents")
    val agents = groundArguments(predicate, AGENT_ROLE, s, topN, threshold)
    println(s"grounding the themes")
    val themes = groundArguments(predicate, THEME_ROLE, s, topN, threshold)
    println(s"grounding the others")
    val others = groundArguments(predicate, OTHER_ROLE, s, topN, threshold)

    groundedPredicate match {
      case prop if prop.isProperty =>
        println("The predicate was a property, so we're switching the themes with it")
        // If the predicate is a property, then the we need to switch the theme and the predicate
        PredicatePackage(themes, agents, Seq(groundedPredicate), others)
      case _ => PredicatePackage(Seq(groundedPredicate), agents, themes, others)
    }


  }

  // Find all arguments from a given predicate of a certain role (e.g., A1, A0, etc) and ground them
  private def groundArguments(predicate: Int, role: String, s:SentenceHelper, topN: Option[Int], threshold: Option[Float]): Seq[GroundedSpan] = {
    s.srls.outgoingEdges(predicate)
      .filter(_._2 == role)
      .flatMap(dst => handlePrepositions(dst, s))
      .map(tok => groundChunk(tok._1, s, topN, threshold))
  }

  // In SRL, currently the arguments that point to a preposition stay there, rather than continuing
  // on to the object of the preposition.  However, when grounding, we want the object.  This traverses to the
  // object(s) of the preposition, if any.
  private def handlePrepositions(step: (Int, String), s: SentenceHelper): Seq[(Int, String)] = {
    val (dst, role) = step
    println(s"Checking for preposition: ${s.words(dst)}")
    s.tokenOrObjOfPreposition(dst).map((_, role))
  }

  // Ground the chunk that the token is in, but in isolation from the rest of the sentence
  private def groundChunk(token: Int, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): GroundedSpan = {
//    val chunkSpan = s.chunkIntervals.collect{ case c if c.contains(token) => c} match {
//      case Seq() =>
//        logger.warn(s"Token $token is not in a chunk.  chunks: ${s.chunks.mkString(", ")}")
//        Interval(token, token + 1)  // if empty, backoff
//      case Seq(chunk) => chunk      // one found, yay! We'll use it
//      case chunks => throw new RuntimeException(s"Chunks have overlapped, there is a problem.  \n\ttoken: $token\n\tchunks: ${chunks.mkString(", ")}")
//    }
//    val trimmedChunk = s.chunkAvoidingSRLs(chunkSpan, token)
    val trimmedChunk = Interval(token, token+1)
    // First check to see if it's a property, if it is, ground as that
    val propertyOpt = maybeProperty(trimmedChunk, s)
    if (propertyOpt.isDefined) {
      val g = GroundedSpan(trimmedChunk, propertyOpt.get, isProperty = true)
      println(s"PROPERTY: grounded <<${s.wordsSliceString(trimmedChunk)}>> as: ${g}")
      g
    } else {
      // Otherwise, ground as either a process or concept
      val g = GroundedSpan(trimmedChunk, groundToBranches(Seq(CONCEPT, PROCESS), trimmedChunk, s.sentence, topN, threshold), isProperty = false)
      println(s"grounded <<${s.wordsSliceString(trimmedChunk)}>> as: ${g}")
      g
    }
  }

  private def maybeProperty(span: Interval, s: SentenceHelper): Option[OntologyGrounding] = {
    val tempGrounding = groundProperty(span, s, topN=Option(1), threshold=Option(0.8f))
    if (tempGrounding.nonEmpty) Option(tempGrounding) else None
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
        case t: TriggeredAttachment if !t.isInstanceOf[Property] => Seq(t.trigger) ++ t.quantifiers.getOrElse(Seq())
        case _: Property => Seq.empty
        case c: ContextAttachment => Seq(c.text)
        case _ => ???
      }
    }
  }

//  private def groundToAllBranches(span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
//    val contentWords = canonicalizer.canonicalWordsFromSentence(s, span).toArray
//    val initialGroundings = groundPatternsThenEmbeddings(contentWords, conceptPatterns, conceptEmbeddings)
//    val filtered = filterAndSlice(initialGroundings, topN, threshold)
//    newOntologyGrounding(filtered)
//  }

  private def groundProperty(span: Interval, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = groundToBranches(Seq(PROPERTY), span, s, topN, threshold)

  private def groundToBranches(branches: Seq[String], span: Interval, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    groundToBranches(branches, span, s.sentence, topN, threshold)
  }
  private def groundToBranches(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    val patterns = branches.flatMap(conceptPatternsSeq(_))
    val embeddings = branches.flatMap(conceptEmbeddingsSeq(_))
    val contentWords = canonicalizer.canonicalWordsFromSentence(s, span).toArray
    val initialGroundings = groundPatternsThenEmbeddings(contentWords, patterns, embeddings)
    val filtered = filterAndSlice(initialGroundings, topN, threshold)
    newOntologyGrounding(filtered)
  }


  // Helper class to hold a bunch of things that are needed throughout the process for grounding
  case class SentenceHelper(sentence: Sentence, tokenInterval: Interval, exclude: Set[String]) {
    val chunks: Array[String] = sentence.chunks.get
    val chunkIntervals: Seq[Interval] = chunkSpans
    val words: Array[String] = sentence.words
    val srls: DirectedGraph[String] = sentence.semanticRoles.get
    val dependencies: DirectedGraph[String] = sentence.dependencies.get
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

    val allTokensInvolvedInPredicates: Seq[Int] = {
      srls.allEdges
        .flatMap(edge => Seq(edge._1, edge._2))
        .flatMap(tokenOrObjOfPreposition)
        .distinct
        .sorted
    }

    def findStart(chunkSpan: Interval, tok: Int): Int = {
      if (chunkSpan.start == tok) {
        tok
      } else {
        val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
        // not the first thing in the sequence
        val startLimit = if (currTokenIdx > 0) {
          allTokensInvolvedInPredicates(currTokenIdx - 1) + 1
        } else chunkSpan.start
        println(s"startLimit: $startLimit")
        val start = math.max(chunkSpan.start, startLimit)
        println(s"start: $start")
        start
      }
    }

    def findEnd(chunkSpan: Interval, tok: Int): Int = {
      if (chunkSpan.end == tok) {
        tok
      } else {
        val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
        // not the first thing in the sequence
        val endLimit = if (currTokenIdx < allTokensInvolvedInPredicates.length - 1) {
          allTokensInvolvedInPredicates(currTokenIdx + 1) // don't need to subtract 1 bc exclusive interval
        } else chunkSpan.end
        println(s"endLimit: $endLimit")
        val end = math.min(chunkSpan.end, endLimit)
        println(s"end: $end")
        end
      }
    }

    def chunkAvoidingSRLs(chunkSpan: Interval, tok: Int): Interval = {
      assert(allTokensInvolvedInPredicates.contains(tok))
      val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
      println(s"Trimming: (tok=$tok, idx=${currTokenIdx})")
      println(allTokensInvolvedInPredicates.mkString(", "))

      // If the tok starts the chunk
      val start = findStart(chunkSpan, tok)
      val end = findEnd(chunkSpan, tok)
      Interval(start, end)
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
    def tokenOrObjOfPreposition(tok: Int): Seq[Int] = {
      if (sentence.tags.get(tok) == "IN") {
        println(s"It's a preposition... (tok=$tok)")
        // if it's a preposition, follow the outgoing `case` dependency edge
        val out = dependencies
          // Get the next tokens that this token links to
          .incomingEdges(tok)
          // Get the subset that corresponds to the `case` dependency
          .collect{ case e if e._2 == "case" => e._1 }
        println(s"Followed to: ${out.map(words(_)).mkString(", ")}")
        out
      } else {
        Seq(tok)
      }
    }

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
