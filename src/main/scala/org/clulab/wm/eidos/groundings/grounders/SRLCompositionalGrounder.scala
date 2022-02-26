package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, GraphMap, Interval}
import org.clulab.wm.eidos.attachments.{ContextAttachment, Property, TriggeredAttachment}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, EidosWordToVec, IndividualGrounding, OntologyGrounding, PredicateGrounding}
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.wm.eidos.groundings.ConceptExamples
import org.clulab.wm.eidos.groundings.OntologyAliases.IndividualGroundings
import org.clulab.wm.eidos.groundings.OntologyNodeGrounding
import org.clulab.wm.eidos.groundings.grounders.SRLCompositionalGrounder.propertyConfidenceThreshold
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.GroundingUtils
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosTokenizer
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.ontologies.DomainOntology

import scala.collection.mutable.ArrayBuffer

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding, isProperty: Boolean = false)

class PredicateTuple protected (
  val theme: OntologyGrounding,
  val themeProperties: OntologyGrounding,
  val themeProcess: OntologyGrounding,
  val themeProcessProperties: OntologyGrounding,
  val predicates: Set[Int]
) {
  def nameAndScore(gr: OntologyGrounding): String = nameAndScore(gr.headOption.get)
  def nameAndScore(gr: IndividualGrounding): String = {
    s"${gr.name} (${gr.score})"
  }

  override def toString: String = {
    val sb = new ArrayBuffer[String]()

    def append(grounding: OntologyGrounding, label: String): Unit = {
      val single = grounding.individualGroundings.take(1)

      if (single.nonEmpty)
        sb.append(label + nameAndScore(single.head))
    }

    append(theme, "THEME: ")
    append(themeProperties, "Theme properties: ")
    append(themeProcess, "THEME PROCESS: ")
    append(themeProcessProperties, "Process properties: ")
    sb.mkString("\n")
  }

  val name: String = {
    if (theme.nonEmpty) {
      val sb = new ArrayBuffer[String]()

      def appendIf(condition: Boolean, f: => String): Unit = if (condition) sb.append(f)

      appendIf(condition = true, s"THEME: ${nameAndScore(theme)}")
      appendIf(themeProperties.nonEmpty, s" , Theme properties: ${themeProperties.take(5).map(nameAndScore).mkString(", ")}")
      appendIf(themeProcess.nonEmpty, s"; THEME PROCESS: ${nameAndScore(themeProcess)}")
      appendIf(themeProcessProperties.nonEmpty, s", Process properties: ${themeProcessProperties.take(5).map(nameAndScore).mkString(", ")}")
      sb.mkString("")
    }
    else
      "Themeless Compositional Grounding"
  }
  val score: Float = {
    val themeScoreOpt = theme.individualGroundings.headOption.map(_.score)
    val themeProcessScoreOpt = themeProcess.individualGroundings.headOption.map(_.score)

    val themePropertyScoreOpt = themeProperties.individualGroundings.headOption.map(_.score * 0.5f)
    val themeProcessPropertyScoreOpt = themeProcessProperties.individualGroundings.headOption.map(_.score * 0.5f)

    val allScores = (themeScoreOpt ++ themeProcessScoreOpt ++ themePropertyScoreOpt ++ themeProcessPropertyScoreOpt).toSeq
    if (allScores.isEmpty) 0.0f
//    else (allScores.toSeq.sum / allScores.toSeq.length)
    else GroundingUtils.noisyOr(allScores)
  }
}

object PredicateTuple {
  def apply(
    theme: OntologyGrounding,
    themeProperties: OntologyGrounding,
    themeProcess: OntologyGrounding,
    themeProcessProperties: OntologyGrounding,
    predicates: Set[Int]
  ): PredicateTuple = new PredicateTuple(
    theme.filterSlots(SRLCompositionalGrounder.CONCEPT),
    themeProperties.filterSlots(SRLCompositionalGrounder.PROPERTY),
    themeProcess.filterSlots(SRLCompositionalGrounder.PROCESS),
    themeProcessProperties.filterSlots(SRLCompositionalGrounder.PROPERTY),
    predicates
  )
}

class SRLCompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer, tokenizer: EidosTokenizer)
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  val emptyPredicateTuple: PredicateTuple = PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, Set.empty)

  lazy val proc: CluProcessor = {
    Utils.initializeDyNet()
    new CluProcessor() {
      // Reuse the EidosTokenizer from the EidosProcess, but replace its wrapped tokenizer with the localTokenizer.
      override lazy val tokenizer: EidosTokenizer = SRLCompositionalGrounder.this.tokenizer.copyWithNewTokenizer(localTokenizer)
    }
  }

  // If we are "regrounding" from documents that were saved as jsonld and deserialized,
  // and the original had no enhanced roles, then there is no record of the related graph.
  // A new, edgeless directed graph is inserted in the sentence to make up for it.
  // New roles are _not_ recalculated here, which might be necessary if we had deserialized
  // an old JsonLD file in which the existing roles just hadn't been serialized.
  def ensureSRLs(sentence: Sentence): Sentence = {
    if (sentence.enhancedSemanticRoles.isEmpty) {
      val enhancedRoles = new DirectedGraph[String](List.empty, Some(sentence.words.length))
      sentence.graphs += GraphMap.ENHANCED_SEMANTIC_ROLES -> enhancedRoles
    }
    sentence
  }

  def inBranch(s: String, branches: Seq[ConceptEmbedding]): Boolean =
    branches.exists(_.namer.getName == s)

  protected lazy val conceptEmbeddingsMap: Map[String, Seq[ConceptEmbedding]] =
    CompositionalGrounder.branches.map { branch =>
      branch -> conceptEmbeddings.filter { _.namer.getBranchOpt.contains(branch) }
    }.toMap

  protected lazy val conceptPatternsMap: Map[String, Seq[ConceptPatterns]] =
    CompositionalGrounder.branches.map { branch =>
      branch -> conceptPatterns.filter { _.namer.getBranchOpt.contains(branch) }
    }.toMap

  protected lazy val conceptExamplesMap: Map[String, Seq[ConceptExamples]] =
    CompositionalGrounder.branches.map { branch =>
      branch -> conceptExamples.filter { _.namer.getBranchOpt.contains(branch) }
    }.toMap

  // primarily used for passing in the canonical name parts
  override def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    throw new RuntimeException("The SRLCompositionalGrounder isn't designed to be used with canonical name parts only.")
  }

  override def groundText(text: String, canonicalNameParts: Array[String]): OntologyGrounding = {
    val doc = proc.annotate(text)
    val groundings = for {
      s <- doc.sentences
      // TODO (at some point) -- the empty sequence here is a placeholder for increase/decrease triggers
      //  Currently we don't have "access" to those here, but that could be changed
      //  Further, the Nones are for a topN and a threshold, which we don't have here
      ontologyGrounding <- groundSentenceSpan(s, 0, s.words.length, Set(), None, None)
      singleGrounding <- ontologyGrounding.individualGroundings
    } yield singleGrounding

    val groundingResult = newOntologyGrounding(groundings.sortBy(- _.score))
    groundingResult
  }

  override def groundEidosMention(mention: EidosMention, topN: Option[Int] = None, threshold: Option[Float] = None): Seq[OntologyGrounding] = {
    if (!EidosOntologyGrounder.groundableType(mention))
      // Do nothing to non-groundable mentions
      Seq(emptyOntologyGrounding)
    else {
      // or else ground them.
      val sentenceObj = ensureSRLs(mention.odinMention.sentenceObj)
      val out = groundSentenceSpan(
        sentenceObj,
        mention.odinMention.start,
        mention.odinMention.end,
        getAttachmentStrings(mention.odinMention),
        topN,
        threshold
      )
      out
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
    topNOpt: Option[Int],
    thresholdOpt: Option[Float]
  ): Seq[OntologyGrounding] = {

    def emptyOrBranchToPredicateTuple(ontologyGrounding: OntologyGrounding): PredicateTuple = {
      if (ontologyGrounding.isEmpty)
      // If there's nothing here, return an empty grounding.
        PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, tokenInterval.toSet)
      else {
        val branch = ontologyGrounding.individualGroundings.head.branchOpt.get

        branch match {
          case SRLCompositionalGrounder.CONCEPT =>
            PredicateTuple(ontologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, tokenInterval.toSet)
          case SRLCompositionalGrounder.PROCESS =>
            PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, ontologyGrounding, emptyOntologyGrounding, tokenInterval.toSet)
        }
      }
    }

    def groundWithoutPredicates(sentenceHelper: SentenceHelper): Seq[PredicateGrounding] = {
      val themePropertyOpt: Option[OntologyGrounding] = maybeProperty(tokenInterval, sentenceHelper)
      // First check for Property
      val predicateTuple =
        if (themePropertyOpt.isDefined)
        // If there is a Property, just return that
          PredicateTuple(emptyOntologyGrounding, themePropertyOpt.get, emptyOntologyGrounding, emptyOntologyGrounding, tokenInterval.toSet)
        else {
          val maybeConceptOrProcess: OntologyGrounding = groundToBranches(SRLCompositionalGrounder.processOrConceptBranches, tokenInterval, s, topNOpt, thresholdOpt)

          emptyOrBranchToPredicateTuple(maybeConceptOrProcess)
        }

      Seq(PredicateGrounding(predicateTuple))
    }

    def findExactPredicateGroundingAndRange(mentionStrings: Array[String], mentionRange: Range): Seq[(PredicateGrounding, Range)] = {
      val exactSingleOntologyNodeGroundingAndRanges = SRLCompositionalGrounder.processOrConceptBranches.flatMap { branch =>
        exactMatchForPreds(mentionStrings, conceptEmbeddingsMap(branch), mentionRange)
      }

      exactSingleOntologyNodeGroundingAndRanges.map { case (ontologyNodeGrounding, range) =>
        val ontologyGrounding = newOntologyGrounding(Seq(ontologyNodeGrounding))
        val exactPredicateGrounding = PredicateGrounding(emptyOrBranchToPredicateTuple(ontologyGrounding))
        (exactPredicateGrounding, range)
      }
    }

    def combineExactAndInexact(exactPredicateGrounding: PredicateGrounding, inexactPredicateGrounding: PredicateGrounding): Option[PredicateGrounding] = {

      def isEmpty(predicateGrounding: PredicateGrounding): Boolean =
          predicateGrounding.predicateTuple.theme.isEmpty &&
          predicateGrounding.predicateTuple.themeProperties.isEmpty &&
          predicateGrounding.predicateTuple.themeProcess.isEmpty &&
          predicateGrounding.predicateTuple.themeProcessProperties.isEmpty

      def option(ontologyGrounding: OntologyGrounding): Option[OntologyGrounding] =
          if (ontologyGrounding.isEmpty) None
          else Some(ontologyGrounding)

      def getSlots(predicateGrounding: PredicateGrounding): Array[Option[OntologyGrounding]] = {
        val predicateTuple = predicateGrounding.predicateTuple
        Array(
          option(predicateTuple.theme),
          option(predicateTuple.themeProperties),
          option(predicateTuple.themeProcess),
          option(predicateTuple.themeProcessProperties)
        )
      }

      def combineSlots(exactSlots: Array[Option[OntologyGrounding]], inexactSlots: Array[Option[OntologyGrounding]]): Array[Option[OntologyGrounding]] =
          exactSlots.indices.map { index =>
            if (exactSlots(index).isDefined) exactSlots(index)
            else if (inexactSlots(index).isDefined) inexactSlots(index)
            else None
          }.toArray

      def newPredicateGrounding(slots: Array[Option[OntologyGrounding]], predicates: Set[Int]): PredicateGrounding = PredicateGrounding(
        PredicateTuple(
          slots(0).getOrElse(emptyOntologyGrounding),
          slots(1).getOrElse(emptyOntologyGrounding),
          slots(2).getOrElse(emptyOntologyGrounding),
          slots(3).getOrElse(emptyOntologyGrounding),
          predicates
        )
      )

      if (isEmpty(exactPredicateGrounding)) None
      else {
        val exactSlots = getSlots(exactPredicateGrounding)
        val inexactSlots = getSlots(inexactPredicateGrounding)
        val differ = exactSlots.indices.exists { index =>
          exactSlots(index).isEmpty && inexactSlots(index).isDefined
        }

        if (!differ) None
        else {
          val combinedSlots = combineSlots(exactSlots, inexactSlots)
          val predicates = exactPredicateGrounding.predicateTuple.predicates ++ inexactPredicateGrounding.predicateTuple.predicates

          Some(newPredicateGrounding(combinedSlots, predicates))
        }
      }
    }

    def findInexactPredicateGroundings(exactPredicateGrounding: PredicateGrounding, exactRange: Range, mentionRange: Range, sentenceHelper: SentenceHelper): Seq[PredicateGrounding] = {
      val inexactPredicateGroundings =
          for (index <- mentionRange if !exactRange.contains(index))
          yield {
            val themePropertyOpt: Option[OntologyGrounding] = maybeProperty(Interval(index), sentenceHelper)
            val predicateTuple =
                if (themePropertyOpt.isDefined)
                  PredicateTuple(emptyOntologyGrounding, themePropertyOpt.get, emptyOntologyGrounding, emptyOntologyGrounding, Set(index))
                else {
                  // isArg if incoming SRL edges AND no outgoing SRL edges
                  val isArg = sentenceHelper.srls.getIncomingEdges(index).nonEmpty && sentenceHelper.srls.getOutgoingEdges(index).isEmpty
                  val conceptProcessOpt =
                      if (isArg) groundToBranches(Seq(SRLCompositionalGrounder.CONCEPT), Interval(index), s, topNOpt, thresholdOpt)
                      else groundToBranches(Seq(SRLCompositionalGrounder.PROCESS), Interval(index), s, topNOpt, thresholdOpt)
                  // isPred if no incoming SRL edges (not connected in SRL graph) OR there are outgoing SRL edges
                  val isPred = sentenceHelper.srls.getIncomingEdges(index).isEmpty || sentenceHelper.srls.getOutgoingEdges(index).nonEmpty

                  if (isPred)
                    PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, conceptProcessOpt, emptyOntologyGrounding, mentionRange.toSet)
                  else
                    PredicateTuple(conceptProcessOpt, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, Set(index))
                }

            PredicateGrounding(predicateTuple)
          }

      val combinedPredicateGroundings = inexactPredicateGroundings.flatMap { inexactPredicateGrounding =>
        combineExactAndInexact(exactPredicateGrounding, inexactPredicateGrounding)
      }
      // Do all the ones that can be combined, then the exact only, then the individual inexact ones.
      val predicateGroundings = (combinedPredicateGroundings :+ exactPredicateGrounding) ++ inexactPredicateGroundings
      val sortedPredicateGroundings = predicateGroundings.sortBy(-_.score)
      val slicedPredicateGroundings = topNOpt.map(sortedPredicateGroundings.take).getOrElse(sortedPredicateGroundings)

      slicedPredicateGroundings
    }

    def groundWithPredicates(sentenceHelper: SentenceHelper, predicates: Seq[Int]): Seq[PredicateGrounding] = {
      val mentionRange = Range(tokenInterval.start, tokenInterval.end)
      val mentionStrings = mentionRange.map(s.lemmas.get(_).toLowerCase).toArray // used to be words, now lemmas
      val exactPredicateGroundingsAndRanges = {
        val exactPredicateGroundingsAndRanges = findExactPredicateGroundingAndRange(mentionStrings, mentionRange)

        if (exactPredicateGroundingsAndRanges.nonEmpty) exactPredicateGroundingsAndRanges
        else Seq((PredicateGrounding(emptyPredicateTuple), Range(0, 0)))
      }
      val exactAndInexact = exactPredicateGroundingsAndRanges.flatMap { case (predicateGrounding, range) =>
        findInexactPredicateGroundings(predicateGrounding, range, mentionRange, sentenceHelper)
      }

      exactAndInexact.sortBy(-_.score)
    }

    val sentenceHelper = SentenceHelper(s, tokenInterval, exclude)
    val validPredicates = sentenceHelper.validPredicates.sorted
    val srlGrounding =
        if (validPredicates.isEmpty) groundWithoutPredicates(sentenceHelper)
        else groundWithPredicates(sentenceHelper, validPredicates)

    Seq(newOntologyGrounding(srlGrounding))
  }

  // TODO: Remove old functions no longer used, or keep for posterity, in case recent changes need rolled back?
  private def packagePredicate(
    pred: Int,
    s: SentenceHelper,
    topN: Option[Int],
    threshold: Option[Float]
  ): Seq[PredicateTuple] = {

    // make theme paths, i.e., the path through the SRL graph from the predicate to the theme leaf
    val graphNode = GraphNode(pred, s, backoff = true, topN, threshold, Set())
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
    val squeezed = squeezeNodeList(path.reverse.filter(_.grounding.nonEmpty), List.empty)
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
          emptyOntologyGrounding,
          emptyOntologyGrounding,
          Set(onlyPredicate.index)
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
          Set(theme.index, process.index)
        )

      // disallow the proces == theme
      case theme :: process :: _ if theme.grounding.headName == process.grounding.headName =>
        val themeScore = theme.grounding.headOption.map(_.score).getOrElse(-100f)
        val processScore = process.grounding.headOption.map(_.score).getOrElse(-100f)
        if (themeScore >= processScore || theme.grounding.individualGroundings.length == 1) {
          PredicateTuple(
            theme.grounding,
            theme.propertyGroundingOrNone,
            // drop the top grounding of the process
            process.grounding.dropFirst(),
            process.propertyGroundingOrNone,
            Set(theme.index, process.index)
          )
        } else {
          PredicateTuple(
            // drop the top grounding of the process
            theme.grounding.dropFirst(),
            theme.propertyGroundingOrNone,
            process.grounding,
            process.propertyGroundingOrNone,
            Set(theme.index, process.index)
          )
        }

      case _ => throw new RuntimeException("Couldn't create predicate from path.")
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
          head.setProperty(next)
          squeezeNodeList(rest, processed :+ head)
        } else {
          squeezeNodeList(next +: rest, processed :+ head)
        }
    }
  }

  // Get the themes of the predicate
  private def getThemes(predicate: Int, s: SentenceHelper, backoff: Boolean, alreadySeen: Set[Int]): Array[Int] = {
    val found = getArguments(predicate, SRLCompositionalGrounder.THEME_ROLE, s)
    if (found.isEmpty && backoff) {
      // Handle "just in case" infinite loop -- seemed to happen earlier, but the algorithm was diff then...
      s.outgoingOfType(predicate, Seq("compound")).filterNot(i => (i == predicate) || alreadySeen.contains(i))
    } else {
      // prevent infinite loops in edge cases
      found.filterNot(i => (i == predicate) || alreadySeen.contains(i))
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
        case _: Property => Seq.empty
        case t: TriggeredAttachment => Seq(t.trigger) ++ t.quantifiers.getOrElse(Seq())
        case c: ContextAttachment => Seq(c.text)
        case _ => throw new RuntimeException("Unexpected attachment")
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
    val tempGrounding = groundProperty(span, s, topN = Some(1), threshold = Some(SRLCompositionalGrounder.propertyConfidenceThreshold))
    if (tempGrounding.nonEmpty) Some(tempGrounding) else None
  }

  private def groundProperty(span: Interval, s: SentenceHelper, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    groundToBranches(SRLCompositionalGrounder.propertyBranch, span, s.sentence, topN, threshold)
  }

  private def groundToBranches(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    val contentWords = canonicalizer.canonicalWordsFromSentence(s, span).toArray
    val branchesGroundings = branches.flatMap { branch =>
      val patterns = conceptPatternsMap(branch)
      val examples = conceptExamplesMap(branch)
      val embeddings = conceptEmbeddingsMap(branch)
      val branchGroundings = groundPatternsThenEmbeddings(contentWords, patterns, examples, embeddings)

      // Do this first for each branch and then
      filterAndSlice(branchGroundings, topN, threshold)
    }
    // at the end, filter the combined values if appropriate.
    val filtered =
        if (branches.length == 1) branchesGroundings
        else filterAndSlice(branchesGroundings, topN, threshold)

    newOntologyGrounding(filtered)
  }

  case class GraphNode(index: Int, sentenceHelper: SentenceHelper, backoff: Boolean, topN: Option[Int], threshold: Option[Float], ancestors: Set[Int]) {
    lazy val groundedSpan: GroundedSpan = groundToken(index, sentenceHelper, topN, threshold)
    lazy val isProperty: Boolean = groundedSpan.isProperty
    val children: Array[GraphNode] = getThemes(index, sentenceHelper, backoff, ancestors).map(GraphNode(_, sentenceHelper, backoff, topN, threshold, ancestors ++ Set(index)))
    protected var propertyOpt: Option[GraphNode] = None

    def grounding: OntologyGrounding = groundedSpan.grounding

    def getPropertyOpt: Option[GraphNode] = propertyOpt

    def setProperty(property: GraphNode): Unit = propertyOpt = Some(property)

    def propertyGroundingOrNone: OntologyGrounding = propertyOpt.map(_.grounding).getOrElse(emptyOntologyGrounding)
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
    // don't use the roots as here we're only interested in outgoing "sources" for predicates,
    // and the roots also contain the unreachable nodes etc.
    val original = srls.edges.map(edge => edge.source)
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
    // val currTokenIdx = allTokensInvolvedInPredicates.indexOf(tok)

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
      // if it's a preposition, follow the outgoing `case` dependency edge
      val out = dependencies
        // Get the next tokens that this token links to
        .incomingEdges(tok)
        // Get the subset that corresponds to the `case` dependency
        .collect{ case e if Seq("case", "mark").contains(e._2) => e._1 }
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
