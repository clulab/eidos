package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.wm.eidos.attachments.{ContextAttachment, Property, TriggeredAttachment}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, EidosWordToVec, IndividualGrounding, OntologyGrounding, PredicateGrounding}
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.wm.eidos.groundings.GroundingUtils
import org.clulab.wm.eidos.groundings.grounders.SRLCompositionalGrounder.propertyConfidenceThreshold
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosTokenizer
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.ontologies.DomainOntology

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding, isProperty: Boolean = false)
case class PredicateTuple(
  theme: OntologyGrounding,
  themeProperties: OntologyGrounding,
  themeProcess: OntologyGrounding,
  themeProcessProperties: OntologyGrounding,
  predicates: Set[Int]
  ) {
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

  lazy val proc: CluProcessor = {
    Utils.initializeDyNet()
    new CluProcessor() {
      // Reuse the EidosTokenizer from the EidosProcess, but replace its wrapped tokenizer with the localTokenizer.
      override lazy val tokenizer: EidosTokenizer = SRLCompositionalGrounder.this.tokenizer.copyWithNewTokenizer(localTokenizer)
    }
  }

  // If we are "regrounding" from documents that were saved as jsonld and deserialized,
  // then the sentence has the wrong kind of graph and the right kind must be recalculated.
  // TODO: rectify this situation.
  def ensureSRLs(sentence: Sentence): Sentence = {
    if (sentence.enhancedSemanticRoles.isDefined)
      sentence
    else {
      SRLCompositionalGrounder.logger.warn("A graph is being recalculated in order to reground a sentence.")
      val document = proc.annotateFromSentences(Seq(sentence.getSentenceText))
      document.sentences.head
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
    if (!EidosOntologyGrounder.groundableType(mention))
      // Do nothing to non-groundable mentions
      Seq(newOntologyGrounding())
    else {
      // or else ground them.
      val sentenceObj = ensureSRLs(mention.odinMention.sentenceObj)
      groundSentenceSpan(
        sentenceObj,
        mention.odinMention.start,
        mention.odinMention.end,
        getAttachmentStrings(mention.odinMention),
        topN,
        threshold
      )
    }
  }

  def groundSentenceSpan(s: Sentence, start: Int, end: Int, exclude: Set[String], topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding] = {
    val tokenInterval = Interval(start, end)
    groundSentenceSpan(s, tokenInterval, exclude, topN, threshold)
  }

  def groundSentenceSpan(
    s: Sentence,
    tokenInterval: Interval,
    exclude: Set[String],
    topN: Option[Int],
    threshold: Option[Float]
  ): Seq[OntologyGrounding] = {

    val sentenceHelper = SentenceHelper(s, tokenInterval, exclude)
    val srlGrounding = sentenceHelper.validPredicates match {

      case Seq() =>
        // No predicates
        // check for properties, if there we'll attach to the pseudo-theme
        val propertyOpt = maybeProperty(tokenInterval, sentenceHelper)
        val themeProperty = propertyOpt.getOrElse(newOntologyGrounding())
        // make a pseudo theme
        // fixme: should we ground the pseudo theme to the process AND concept branches
        val pseudoTheme = groundToBranches(SRLCompositionalGrounder.pseudoThemeBranches, tokenInterval, s, topN, threshold)
        val predicateTuple = PredicateTuple(pseudoTheme, themeProperty, newOntologyGrounding(), newOntologyGrounding(), tokenInterval.toSet)
        Seq(PredicateGrounding(predicateTuple))

      case preds =>
        val groundings = new ArrayBuffer[PredicateGrounding]
        var seen = Set[Int]()
        for (p <- preds) {
          if (!seen.contains(p)) {
            val predicateTuples = packagePredicate(p, sentenceHelper, topN, threshold)
            for (tuple <- predicateTuples) {
              // Create a grounding from the package and add it to the list
              groundings.append(PredicateGrounding(tuple))
              // Add all the predicates covered here
              seen ++= tuple.predicates
            }
          }
        }

        // Sort them highest first and take the top N if applicable
        val sortedSliced = groundings.sortBy(-_.score)
        sortedSliced.take(topN.getOrElse(sortedSliced.length))
    }

    Seq(newOntologyGrounding(srlGrounding))
  }

  private def packagePredicate(
    pred: Int,
    s: SentenceHelper,
    topN: Option[Int],
    threshold: Option[Float],
  ): Seq[PredicateTuple] = {

    // make theme paths, i.e., the path through the SRL graph from the predicate to the theme leaf
    val graphNode = GraphNode(pred, s, backoff = true, topN, threshold)
    val themePaths: List[List[GraphNode]] = enumeratePaths(graphNode.children, List(List(graphNode)))
    themePaths.map(path => createPredicateTupleFromPath(path)).sortBy(-_.score)
  }

  // Turn the path from predicate to theme leaf into a PredicateTuple
  def createPredicateTupleFromPath(path: List[GraphNode]): PredicateTuple = {
    // here we iterate through the path (in reverse, i.e. deepest first) and
    // if there are properties, we "attach" them to what they are modifying and
    // remove them from the list.
    // As a result, the only things left in squeezed should _not_ be properties
    // unless there were only properties, etc.)
    val squeezed = squeezeNodeList(path.reverse, List.empty)
    // Now that properties have been "attached", we can operate on the remaining
    // elements of the path more straightforwardly
    squeezed match {
      // If there is nothing, there's no grounding
      case List() => emptyPredicateTuple

      // if there is only one thing in the list, then the predicate had no theme
      // so we promote it.
      case List(onlyPredicate) =>
        PredicateTuple(
          onlyPredicate.grounding,
          onlyPredicate.propertyGroundingOrNone,
          newOntologyGrounding(),
          newOntologyGrounding(),
          Set(onlyPredicate.idx)
        )

      // if there are two or more things in the list,
      // use the deepest as the theme, the next as the process, and ignore the rest
      // since we can't handle more than 2 with the 4-tuple representation
      case theme :: process :: _  if theme.grounding.headName != process.grounding.headName =>
        PredicateTuple(
          theme.grounding,
          theme.propertyGroundingOrNone,
          process.grounding,
          process.propertyGroundingOrNone,
          Set(theme.idx, process.idx)
        )

      // disallow the proces == theme
      case theme :: process :: _ if theme.grounding.headName == process.grounding.headName =>
        val themeScore = theme.grounding.headOption.map(_.score).getOrElse(-100f)
        val processScore = process.grounding.headOption.map(_.score).getOrElse(-100f)
        if (themeScore >= processScore) {
          PredicateTuple(
            theme.grounding,
            theme.propertyGroundingOrNone,
            // drop the top grounding of the process
            process.grounding.dropFirst(),
            process.propertyGroundingOrNone,
            Set(theme.idx, process.idx)
          )
        } else {
          PredicateTuple(
            // drop the top grounding of the process
            theme.grounding.dropFirst(),
            theme.propertyGroundingOrNone,
            process.grounding,
            process.propertyGroundingOrNone,
            Set(theme.idx, process.idx)
          )
        }

      // shouldn't happen
      case _ => ???
    }
  }

  // Iterate through the path, removing properties and adding them to the nodes they modify
  def squeezeNodeList(nodes: List[GraphNode], processed: List[GraphNode]): List[GraphNode] = {
    nodes match {
      // if the list is empty, we're done
      case List() => processed
      // if there's only one thing left, it can't have a property, keep it and stop
      case List(last) => processed :+ last
      // if everything left is a property, cut and run, keep the first bc idk...
      case allProps if allProps.forall(_.isProperty) => processed :+ allProps.head
      // if there are at least 2 things left...
      case head :: next :: rest =>
        if (next.isProperty) {
          head.updateProperty(next)
          squeezeNodeList(rest, processed :+ head)
        } else {
          squeezeNodeList(next +: rest, processed :+ head)
        }
    }
  }

  // Get the themes of the predicate
  private def getThemes(predicate: Int, s: SentenceHelper, backoff: Boolean): Array[Int] = {
    val found = getArguments(predicate, SRLCompositionalGrounder.THEME_ROLE, s)
    if (found.isEmpty && backoff) {
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
      .map(_._1)
  }

  // Get the triggers, quantifiers, and context phrases of the attachments
  private def getAttachmentStrings(mention: Mention): Set[String] = {
    mention.attachments.flatMap { a =>
      a match {
        case t: TriggeredAttachment if !t.isInstanceOf[Property] => Seq(t.trigger) ++ t.quantifiers.getOrElse(Seq())
        case _: Property => Seq.empty
        case c: ContextAttachment => Seq(c.text)
        case _ => ???
      }
    }
  }

  // In SRL, currently the arguments that point to a preposition stay there, rather than continuing
  // on to the object of the preposition.  However, when grounding, we want the object.  This traverses to the
  // object(s) of the preposition, if any.
  private def handlePrepositions(step: (Int, String), s: SentenceHelper): Seq[(Int, String)] = {
    val (dst, role) = step
    s.tokenOrObjOfPreposition(dst).map((_, role))
  }

  // Ground the token, in isolation from the rest of the sentence
  // fixme: we should add the rest of the sentence as context
  private def groundToken(token: Int, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): GroundedSpan = {
    val tokenInterval = Interval(token, token+1)
    // First check to see if it's a property, if it is, ground as that
    val propertyOpt = maybeProperty(tokenInterval, s)
    if (propertyOpt.isDefined) {
      GroundedSpan(tokenInterval, propertyOpt.get, isProperty = true)
    } else {
      // Otherwise, ground as either a process or concept
      GroundedSpan(tokenInterval, groundToBranches(SRLCompositionalGrounder.processOrConceptBranches, tokenInterval, s.sentence, topN,
        threshold), isProperty = false)
    }
  }

  // try to ground the span as a property, returning None if the confidence isn't high enough
  private def maybeProperty(span: Interval, s: SentenceHelper): Option[OntologyGrounding] = {
    val tempGrounding = groundProperty(span, s, topN=Option(1), threshold=Option(propertyConfidenceThreshold))
    if (tempGrounding.nonEmpty) Option(tempGrounding) else None
  }

  private def groundProperty(span: Interval, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    groundToBranches(SRLCompositionalGrounder.propertyBranch, span, s.sentence, topN, threshold)
  }

  private def groundToBranches(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    val patterns = branches.flatMap(conceptPatternsSeq(_))
    val embeddings = branches.flatMap(conceptEmbeddingsSeq(_))
    val contentWords = canonicalizer.canonicalWordsFromSentence(s, span).toArray
    val initialGroundings = groundPatternsThenEmbeddings(contentWords, patterns, embeddings)
    val filtered = filterAndSlice(initialGroundings, topN, threshold)
    newOntologyGrounding(filtered)
  }

  def emptyPredicateTuple: PredicateTuple = PredicateTuple(newOntologyGrounding(), newOntologyGrounding(), newOntologyGrounding(), newOntologyGrounding(), Set.empty)

  case class GraphNode(idx: Int, s: SentenceHelper, backoff: Boolean, topN: Option[Int], threshold: Option[Float]) {
    lazy val groundedSpan: GroundedSpan = groundToken(idx, s, topN, threshold)
    lazy val isProperty: Boolean = groundedSpan.isProperty
    def grounding: OntologyGrounding = groundedSpan.grounding
    val children: Array[GraphNode] = getThemes(idx, s, backoff).map(GraphNode(_, s, backoff, topN, threshold))
    private var _property: Option[GraphNode] = None

    def property: Option[GraphNode] = _property
    def updateProperty(prop: GraphNode) = _property = Some(prop)
    def propertyGroundingOrNone: OntologyGrounding = {
      if (_property.isDefined) {
        _property.get.grounding
      } else {
        newOntologyGrounding()
      }
    }
  }

  // lookup table [Int, Grounding]
  def enumeratePaths(children: Array[GraphNode], traversed: List[List[GraphNode]]): List[List[GraphNode]] = {
    children match {
      case Array() =>
        // add the curr to the end of each list
        traversed

      case cc =>
        // for each path in traversed, for each child, make a new path that is the prev + child
        val branches = for {
          t <- traversed
          c <- cc

        } yield {
          // break loops
          if (!t.contains(c)) {
            enumeratePaths(c.children, traversed.map(_ :+ c))
          } else {
            enumeratePaths(Array.empty, traversed)
          }
        }
        //
        branches.flatten
    }
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
      .filter(tokenInterval.contains)
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

object SRLCompositionalGrounder extends Logging {
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
  val ENTITY = "entity"

  // Groundable Branches
  val pseudoThemeBranches = Seq(CONCEPT, ENTITY)
  val processOrConceptBranches = Seq(CONCEPT, ENTITY, PROCESS)
  val propertyBranch = Seq(PROPERTY)

//  val verbConfidenceThreshold: Float = 0.7f
  val propertyConfidenceThreshold: Float = 0.85f
}


















