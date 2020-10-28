package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.attachments.{ContextAttachment, Property, TriggeredAttachment}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, DomainOntology, EidosWordToVec, IndividualGrounding, OntologyGrounding, PredicateGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{Canonicalizer, GroundingUtils}
import org.slf4j.{Logger, LoggerFactory}
import SRLCompositionalGrounder._
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.wm.eidos.EidosTokenizer

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding, isProperty: Boolean = false)
case class PredicateTuple(theme: OntologyGrounding, themeProperties: OntologyGrounding, themeProcess: OntologyGrounding, themeProcessProperties: OntologyGrounding, predicates: Set[Int]) {

  def nameAndScore(gr: OntologyGrounding): String = nameAndScore(gr.headOption.get)
  def nameAndScore(gr: IndividualGrounding): String = {
    s"${gr.name} (${gr.score})"
  }

  val name: String = {
    if (theme.nonEmpty) {
      val sb = new ArrayBuffer[String]()
      sb.append(s"THEME: ${nameAndScore(theme)}")
      if (themeProperties.nonEmpty) {
       sb.append(s" , Theme properties: ${themeProperties.take(5).map(nameAndScore).mkString(", ")}")
      }
      if (themeProcess.nonEmpty) {
        sb.append(s"; THEME PROCESS: ${nameAndScore(themeProcess)}")
      }
      if (themeProcessProperties.nonEmpty) {
        sb.append(s", Process properties: ${themeProcessProperties.take(5).map(nameAndScore).mkString(", ")}")
      }
      sb.mkString("")
    } else {
      "Empty Compositional Grounding"
    }
  }
  val score: Float = {
    val themeScore = theme.grounding.headOption.map(_.score)
    val themeProcessScore = themeProcess.grounding.headOption.map(_.score)
    val allScores = (themeScore ++ themeProcessScore).toSeq
    if (allScores.isEmpty) 0.0f
//    else (allScores.toSeq.sum / allScores.toSeq.length)
    else GroundingUtils.noisyOr(allScores)
  }
}

class SRLCompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer, tokenizer: EidosTokenizer)
  extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  lazy val proc = {
    Utils.initializeDyNet()
    new CluProcessor() {
      // Reuse the EidosTokenizer from the EidosProcess, but replace its wrapped tokenizer with the localTokenizer.
      override lazy val tokenizer = SRLCompositionalGrounder.this.tokenizer.copyWithNewTokenizer(localTokenizer)
    }
  }

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
    // Do nothing to non-groundable mentions
    if (!EidosOntologyGrounder.groundableType(mention))
      Seq(newOntologyGrounding())
    // or else ground them.
    else {
      val reParsed = proc.annotate(mention.odinMention.sentenceObj.getSentenceText)
      // The same tokenizer should get the same number of sentences.
      val oldSentenceLength = 1
      val newSentenceLength = reParsed.sentences.length

      if (oldSentenceLength == newSentenceLength) {
        val oldWordsLength = mention.odinMention.sentenceObj.words.length
        val newWordsLength = reParsed.sentences.head.words.length

        // The same number of words should also be found.  If not, then the token indexes of the one tokenization
        // will not align with the other tokenization and all further bets are off.  In the worst case, there will
        // be a bounds error on an index and an exception.
        if (oldWordsLength == newWordsLength)
          groundSentenceSpan(reParsed.sentences.head, mention.odinMention.start, mention.odinMention.end, attachmentStrings(mention.odinMention), topN, threshold)
        else
          Seq.empty
      }
      else
        Seq.empty
    }
  }

  def groundSentenceSpan(s: Sentence, start: Int, end: Int, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding] = {
    val tokenInterval = Interval(start, end)
    groundSentenceSpan(s, tokenInterval, exclude, topN, threshold)
  }

  // todo: am I using exclude right?
  def groundSentenceSpan(s: Sentence, tokenInterval: Interval, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding] = {
    val sentenceHelper = SentenceHelper(s, tokenInterval, exclude)
    val srlGrounding = sentenceHelper.validPredicates match {
      case Seq() =>
        // No predicates
        // check for properties, if there we'll attach to the pseudo-theme
        val propertyOpt = maybeProperty(tokenInterval, sentenceHelper)
        val themeProperty = propertyOpt.getOrElse(newOntologyGrounding())
        // make a pseudo theme
        // fixme: should we ground the pseudo theme to the process AND concept branches
        val pseudoTheme = groundToBranches(Seq(CONCEPT), tokenInterval, s, topN, threshold)
        val predicateTuple = PredicateTuple(pseudoTheme, themeProperty, newOntologyGrounding(), newOntologyGrounding(), tokenInterval.toSet)
        Seq(PredicateGrounding(predicateTuple))
      case preds =>
        val groundings = new ArrayBuffer[PredicateGrounding]
        var seen = Set[Int]()
        for (p <- preds) {
//          if (!seen.contains(p)) {
            val predicateTuple = packagePredicate(p, Seq(), sentenceHelper, topN, threshold, Set())
            groundings.append(PredicateGrounding(predicateTuple))
            // Add all the predicates covered here
            seen ++= predicateTuple.predicates
//          }
        }
        // Sort them highest first and take the top N if applicable
        val sortedSliced = groundings.sortBy(-_.score)
        sortedSliced.take(topN.getOrElse(sortedSliced.length))

    }

    Seq(newOntologyGrounding(srlGrounding))

    // Am I done here? or do I need to filter and slice?
  }

  @tailrec
  private def packagePredicate(pred: Int, attachedProperties: Seq[OntologyGrounding], s: SentenceHelper, topN: Option[Int], threshold: Option[Float], predicatesCovered: Set[Int]): PredicateTuple = {
    val themes = getThemes(pred, s, backoff = true).sortBy(s.minGraphDistanceToSyntacticRoot)
    // fixme: is THIS a problem? how often are there multiple themes???
    val theme = themes.headOption

    // Check to see if the predicate is a Property
    val propertyOpt = maybeProperty(Interval(pred, pred + 1), s)
    if (propertyOpt.isEmpty) {
      // It's not a property
      val groundedPred = groundChunk(pred, s, topN, threshold)
      val groundedAttachedProps = attachedProperties.headOption.getOrElse(newOntologyGrounding())
      if (theme.isDefined) {
        // If there's a theme, it occupies the theme position in the tuple
        val (groundedTheme, groundedThemeProps) = tupelize(theme.get, s, topN, threshold)
        PredicateTuple(groundedTheme, groundedThemeProps, groundedPred.grounding, groundedAttachedProps, predicatesCovered ++ Set(pred, theme.get))
      } else {
        // Promote the predicate
        PredicateTuple(groundedPred.grounding, groundedAttachedProps, newOntologyGrounding(), newOntologyGrounding(), predicatesCovered ++ Set(pred))
      }
    }
    else {
      // If the predicate was a property, it is "demoted" to a process property
      if (theme.nonEmpty) {
        packagePredicate(theme.get, attachedProperties ++ Seq(propertyOpt.get), s, topN, threshold, predicatesCovered ++ Set(pred, theme.get))
      } else {
        // property and no theme -- promote the property
        PredicateTuple(propertyOpt.get, newOntologyGrounding(), newOntologyGrounding(), newOntologyGrounding(), predicatesCovered++ Set(pred))
      }
    }
  }


  def tupelize(tok: Int, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): (OntologyGrounding, OntologyGrounding) = {

    val propertyOpt = maybeProperty(Interval(tok, tok + 1), s)
    if (propertyOpt.isDefined) {
      // it's a property, check if there's a theme (recall in SRL these properties are "predicates")
      val themes = getThemes(tok, s, backoff = false)
      if (themes.nonEmpty) {
        // there is a theme
        val groundedThemes = themes
          .map(t => groundChunk(t, s, topN, threshold))
          .sortBy(_.grounding.headOption.map(_.score).getOrElse(-1.0f))
        // Since we know there's at least one theme, head here is safe
        val bestGroundedTheme = groundedThemes.head
        (bestGroundedTheme.grounding, propertyOpt.get)
      } else {
        // there is no theme, promote the property???
        (propertyOpt.get, newOntologyGrounding())
      }
    } else {
      // the token is not a property
      val groundedTok = groundChunk(tok, s, topN, threshold)
      (groundedTok.grounding, newOntologyGrounding())
    }
  }

  private def getThemes(predicate: Int, s: SentenceHelper, backoff: Boolean): Array[Int] = {
    val found = getArguments(predicate, THEME_ROLE, s)
    if (found.isEmpty && backoff) {
      //val other = getArguments(predicate, OTHER_ROLE, s).toSet
      // Handle "just in case" infinite loop -- seemed to happen earlier, but the algorithm was diff then...
      s.outgoingOfType(predicate, Seq("compound")).filterNot(_ == predicate)
    } else {
      // prevent infinite loops in edge cases
      found.filterNot(_ == predicate)
    }
  }

  private def getArguments(predicate: Int, role: String, s: SentenceHelper): Array[Int] = {
    if (predicate >= s.srls.outgoingEdges.length) return Array()
    s.srls.outgoingEdges(predicate)
      .filter(edge => edge._2 == role)
      .filter(edge => s.tokenInterval.contains(edge._1))
//      .flatMap(handlePrepositions(_, s))
      .map(_._1)
  }

  // In SRL, currently the arguments that point to a preposition stay there, rather than continuing
  // on to the object of the preposition.  However, when grounding, we want the object.  This traverses to the
  // object(s) of the preposition, if any.
  private def handlePrepositions(step: (Int, String), s: SentenceHelper): Seq[(Int, String)] = {
    val (dst, role) = step
//    println(s"Checking for preposition: ${s.words(dst)}")
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
      GroundedSpan(trimmedChunk, propertyOpt.get, isProperty = true)
    } else {
      // Otherwise, ground as either a process or concept
      GroundedSpan(trimmedChunk, groundToBranches(Seq(CONCEPT, PROCESS), trimmedChunk, s.sentence, topN, threshold), isProperty = false)
    }
  }

  private def maybeProperty(span: Interval, s: SentenceHelper): Option[OntologyGrounding] = {
    val tempGrounding = groundProperty(span, s, topN=Option(1), threshold=Option(0.85f))
    if (tempGrounding.nonEmpty) Option(tempGrounding) else None
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
}

// Helper class to hold a bunch of things that are needed throughout the process for grounding
case class SentenceHelper(sentence: Sentence, tokenInterval: Interval, exclude: Set[String]) {
  val chunks: Array[String] = sentence.chunks.get
  val chunkIntervals: Seq[Interval] = chunkSpans
  val words: Array[String] = sentence.words
  val srls: DirectedGraph[String] = sentence.enhancedSemanticRoles.get
  val dependencies: DirectedGraph[String] = sentence.dependencies.get
  // The roots of the SRL graph that are within the concept being grounded and aren't part of
  // an something we're ignoring (e.g., increase/decrease/quantification)
  val validPredicates: Seq[Int] = {
    val original = srls.roots.toSeq
      // keep only predicates that are within the mention
      .filter(tokenInterval contains _)
      // remove the predicates which correspond to our increase/decrease/quantifiers
      .filterNot(exclude contains words(_))
    // add back in ones that SRL "missed"
    val corrected = for {
      i <- tokenInterval
      if !original.contains(i)
      if outgoingOfType(i, Seq("compound")).nonEmpty
    } yield i
    // start with those closest to the syntactic root of the sentence to begin with "higher level" predicates
    (original ++ corrected).sortBy(minGraphDistanceToSyntacticRoot)
  }

  // Find the shortest distance (in the syntax graph) between a given token and any of the roots
  def minGraphDistanceToSyntacticRoot(token: Int): Int = {
    // Get the sentence roots -- there can be more than one
    val roots = dependencies.roots
    if (roots.isEmpty) return 0
    // for each, check the shortest path from that root to the given token
    val pathLengths = roots.map(root => dependencies.shortestPath(root, token, ignoreDirection = true).length)
    // select the shortest to be the distance from the token to any of the roots
    pathLengths.min
  }

  val allTokensInvolvedInPredicates: Seq[Int] = {
    srls.allEdges
      .flatMap(edge => Seq(edge._1, edge._2))
//      .flatMap(tokenOrObjOfPreposition)
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
      val start = math.max(chunkSpan.start, startLimit)
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
      val end = math.min(chunkSpan.end, endLimit)
      end
    }
  }

  def chunkAvoidingSRLs(chunkSpan: Interval, tok: Int): Interval = {
    assert(allTokensInvolvedInPredicates.contains(tok))
    val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)
//    println(s"Trimming: (tok=$tok, idx=${currTokenIdx})")
//    println(allTokensInvolvedInPredicates.mkString(", "))

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
        .collect{ case e if Seq("case", "mark").contains(e._2) => e._1 }
      println(s"Followed to: ${out.map(words(_)).mkString(", ")}")
      out
    } else {
      Seq(tok)
    }
  }

  def outgoingOfType(tok: Int, constraints: Seq[String]): Array[Int] = {
    dependencies.outgoingEdges(tok)
      // keep only the ones that satisfy the constraints
      .filter(edge => constraints.contains(edge._2))
      // return the dsts
      .map(_._1)
  }
}


object SRLCompositionalGrounder{

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  // Semantic Roles
  val AGENT_ROLE = "A0"
  val THEME_ROLE = "A1"
  val OTHER_ROLE = "Ax"
  val TIME_ROLE = "AM-TMP"
  val LOC_ROLE = "AM-LOC" // FIXME: may need offline processing to make happen

  // Compositional Ontology Branches
  val PROCESS = "process"
  val CONCEPT = "concept"
  val PROPERTY = "property"

}
