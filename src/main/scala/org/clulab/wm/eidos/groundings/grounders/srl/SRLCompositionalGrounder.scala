package org.clulab.wm.eidos.groundings.grounders.srl

import org.clulab.dynet.Utils
import org.clulab.odin.Mention
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.ContextAttachment
import org.clulab.wm.eidos.attachments.Property
import org.clulab.wm.eidos.attachments.TriggeredAttachment
import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.ConceptExamples
import org.clulab.wm.eidos.groundings.ConceptPatterns
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.IndividualGrounding
import org.clulab.wm.eidos.groundings.OntologyAliases.IndividualGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.PredicateGrounding
import org.clulab.wm.eidos.groundings.grounders.CompositionalGrounder
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.GroundingUtils
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosTokenizer
import org.clulab.wm.eidoscommon.utils.Collection
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.ontologies.DomainOntology

case class GroundedSpan(tokenInterval: Interval, grounding: OntologyGrounding, isProperty: Boolean = false)

class PredicateTuple protected (
  val theme: OntologyGrounding,
  val themeProperties: OntologyGrounding,
  val themeProcess: OntologyGrounding,
  val themeProcessProperties: OntologyGrounding,
  val predicates: Set[Int]
) extends IndexedSeq[OntologyGrounding] {
  val score: Float = {
    val allScores = indices.map(getScoreOpt).flatten
    GroundingUtils.noisyOr(allScores)
  }
  val labelers: Array[Unit => String] = Array(
    { _ => nameAndScore(theme) },
    { _ => themeProperties.take(5).map(nameAndScore).mkString(", ") },
    { _ => nameAndScore(themeProcess) },
    { _ => themeProcessProperties.take(5).map(nameAndScore).mkString(", ") }
  )

  override def length: Int = 4

  def apply(index: Int): OntologyGrounding = {
    index match {
      case 0 => theme
      case 1 => themeProperties
      case 2 => themeProcess
      case 3 => themeProcessProperties
    }
  }

  def getScoreOpt(index: Int): Option[Float] = this(index).individualGroundings.headOption.map(_.score * PredicateTuple.weights(index))

  def nameAndScore(index: Int): String = nameAndScore(this(index))

  def nameAndScore(ontologyGrounding: OntologyGrounding): String = nameAndScore(ontologyGrounding.headOption.get)

  def nameAndScore(individualGrounding: IndividualGrounding): String = s"${individualGrounding.name} (${individualGrounding.score})"

  def indexToStringOpt(index: Int): Option[String] = this(index)
      .individualGroundings
      .headOption
      .map(PredicateTuple.labels(index) + nameAndScore(_))

  override def toString: String = indices
      .map(indexToStringOpt)
      .flatten
      .mkString("\n")

  val name: String = {
    var hasSome = false

    def someIf(grounding: OntologyGrounding, label: String, separator: String, labeler: Unit => String): Option[String] = {
      if (grounding.isEmpty) None
      else {
        val string = (if (hasSome) separator else "") + label + labeler()
        hasSome = true
        Some(string)
      }
    }

    indices
        .map { index =>
          someIf(this(index), PredicateTuple.labels(index), PredicateTuple.separators(index), labelers(index))
        }
        .flatten
        .mkString("")
  }
}

object PredicateTuple {
  val labels = Array(
    "THEME: ",
    "Theme properties: ",
    "THEME PROCESS: ",
    "Process properties: "
  )
  val separators = Array("; ", ", ", "; ", ", ")
  val weights = Array(1f, 0.6f, 1f, 0.6f)

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

  def apply(ontologyGroundings: Array[OntologyGrounding], predicates: Set[Int]): PredicateTuple = {
    require(ontologyGroundings.length == 4)
    apply(ontologyGroundings(0), ontologyGroundings(1), ontologyGroundings(2), ontologyGroundings(3), predicates)
  }

  def apply(
      ontologyGrounding1Opt: Option[OntologyGrounding],
      ontologyGrounding2Opt: Option[OntologyGrounding],
      ontologyGrounding3Opt: Option[OntologyGrounding],
      ontologyGrounding4Opt: Option[OntologyGrounding],
      emptyOntologyGrounding: OntologyGrounding
  ): PredicateTuple = {
    apply(
      ontologyGrounding1Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding2Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding3Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding4Opt.getOrElse(emptyOntologyGrounding),
      Set.empty[Int]
    )
  }
}

class SRLCompositionalGrounder(name: String, domainOntology: DomainOntology, w2v: EidosWordToVec, canonicalizer: Canonicalizer, tokenizer: EidosTokenizer)
    extends EidosOntologyGrounder(name, domainOntology, w2v, canonicalizer) {

  val emptyPredicateTuple: PredicateTuple = PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, Set.empty[Int])

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

    def toPredicateTupleOptWithBranch(ontologyGrounding: OntologyGrounding, branch: String): Option[PredicateTuple] = {
      if (ontologyGrounding.isEmpty) None
      else
        branch match {
          case SRLCompositionalGrounder.CONCEPT =>
            Some(PredicateTuple(ontologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, tokenInterval.toSet))
          case SRLCompositionalGrounder.PROCESS =>
            Some(PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, ontologyGrounding, emptyOntologyGrounding, tokenInterval.toSet))
          case _ => None
        }
    }

    def toPredicateTupleOpt(ontologyGrounding: OntologyGrounding): Option[PredicateTuple] = {
      if (ontologyGrounding.individualGroundings.isEmpty)
        None
      else {
        val branch = ontologyGrounding.individualGroundings.head.branchOpt.get

        toPredicateTupleOptWithBranch(ontologyGrounding, branch)
      }
    }

    def groundWithoutPredicates(sentenceHelper: SentenceHelper): Seq[PredicateGrounding] = {
      val themePropertyOpt: Option[OntologyGrounding] = maybeProperty(tokenInterval, sentenceHelper)
      // First check for Property
      val predicateTupleOpt =
        if (themePropertyOpt.isDefined)
          // If there is a Property, just return that
          Some(PredicateTuple(emptyOntologyGrounding, themePropertyOpt.get, emptyOntologyGrounding, emptyOntologyGrounding, tokenInterval.toSet))
        else {
          val maybeConceptOrProcess: OntologyGrounding = groundToBranches(SRLCompositionalGrounder.processOrConceptBranches, tokenInterval, s, topNOpt, thresholdOpt)

          toPredicateTupleOpt(maybeConceptOrProcess)
        }

      predicateTupleOpt.map(PredicateGrounding).toSeq
    }

    def findExactPredicateGroundingAndRanges(mentionStrings: Array[String], mentionRange: Range): Seq[(PredicateGrounding, Range, String)] = {
      SRLCompositionalGrounder.processOrConceptBranches.flatMap { branch =>
        val exactMatches = exactMatchesForPreds(mentionStrings, conceptEmbeddingsMap(branch), mentionRange)
        exactMatches.flatMap { case (ontologyNodeGrounding, range) =>
          val ontologyGrounding = newOntologyGrounding(Seq(ontologyNodeGrounding))
          toPredicateTupleOptWithBranch(ontologyGrounding, branch).map { predicateTuple =>
            val exactPredicateGrounding = PredicateGrounding(predicateTuple)
            (exactPredicateGrounding, range, branch)
          }
        }
      }
    }

    def combineExactAndInexact(exactPredicateGrounding: PredicateGrounding, inexactPredicateGrounding: PredicateGrounding): Option[PredicateGrounding] = {

      def isEmpty(predicateGrounding: PredicateGrounding): Boolean = predicateGrounding.predicateTuple.forall(_.isEmpty)

      def option(ontologyGrounding: OntologyGrounding): Option[OntologyGrounding] =
          if (ontologyGrounding.isEmpty) None
          else Some(ontologyGrounding)

      def getSlotOpts(predicateGrounding: PredicateGrounding): Array[Option[OntologyGrounding]] =
          predicateGrounding.predicateTuple.map(option).toArray

      def combineSlots(exactSlotOpts: Array[Option[OntologyGrounding]], inexactSlotOpts: Array[Option[OntologyGrounding]]): Array[Option[OntologyGrounding]] =
          exactSlotOpts.zip(inexactSlotOpts).map { case (exactSlotOpt, inexactSlotOpt) =>
            if (exactSlotOpt.isDefined) exactSlotOpt
            else if (inexactSlotOpt.isDefined) inexactSlotOpt
            else None
          }

      def newPredicateGrounding(slotOpts: Array[Option[OntologyGrounding]], predicates: Set[Int]): PredicateGrounding = {
        val ontologyGroundings = slotOpts.map(_.getOrElse(emptyOntologyGrounding))
        PredicateGrounding(PredicateTuple(ontologyGroundings, predicates))
      }

      if (isEmpty(exactPredicateGrounding)) None
      else {
        val exactSlotOpts = getSlotOpts(exactPredicateGrounding)
        val inexactSlots = getSlotOpts(inexactPredicateGrounding)
        val differ = exactSlotOpts.indices.exists { index =>
          exactSlotOpts(index).isEmpty && inexactSlots(index).isDefined
        }

        if (!differ) None
        else {
          val combinedSlots = combineSlots(exactSlotOpts, inexactSlots)
          val predicates = exactPredicateGrounding.predicateTuple.predicates ++ inexactPredicateGrounding.predicateTuple.predicates

          Some(newPredicateGrounding(combinedSlots, predicates))
        }
      }
    }

    def boolsToInts(bools: IndexedSeq[Boolean]): IndexedSeq[Int] = {
      var count = 0

      bools.map { value =>
        if (!value) 0
        else {
          count += 1
          count
        }
      }
    }

    def indexAndCountToCount(position: Int, leftIndexAndCountOpt: Option[(Int, Int)], rightIndexAndCountOpt: Option[(Int, Int)]): Int = {
      (leftIndexAndCountOpt, rightIndexAndCountOpt) match {
        case (None, None) => 0
        case (None, Some((_, rightCount))) => rightCount
        case (Some((_, leftCount)), None) => leftCount
        case (Some((leftIndex, leftCount)), Some((rightIndex, rightCount))) =>
          // Whichever is closest to the position and when tied, the value that
          // is higher than position because we favor right, even though it might
          // be the higher numbered arg or pred.
          val leftDist = math.abs(position - leftIndex)
          val rightDist = math.abs(position - rightIndex)
          if (leftDist < rightDist) leftCount
          else if (leftDist == rightDist) {
            // These should not ever be equal because then there would be an
            // arg and pred at the same position.
            if (leftIndex > rightIndex) leftIndex
            else rightIndex
          }
          else rightCount
      }
    }

    def getBest(indexes: Seq[Int], ontologyGroundingOpts: Seq[Option[OntologyGrounding]]): Option[OntologyGrounding] = {
      if (indexes.isEmpty) None
      else {
        val ontologyGroundings = indexes.map(ontologyGroundingOpts(_).get)
        Some(ontologyGroundings.maxBy(_.individualGroundings.head.score))
      }
    }

    def findWhereAndWhatOptAround[T](values: IndexedSeq[T], position: Int)(f: T => Boolean): Option[(Int, T)] = {
      val whereAndWhatOptBefore = Collection.findWhereAndWhatOptBefore(values, position)(f)
      val whereAndWhatOptAfter = Collection.findWhereAndWhatOptAfter(values, position)(f)

      ((whereAndWhatOptBefore, whereAndWhatOptAfter)) match {
        case (None, None) => None
        case (None, some) => some
        case (some, None) => some
        case (Some(before), Some(after)) =>
          // Prefer the one after in case of ties.
          if (math.abs(after._1 - position) <= math.abs(before._1 - position)) Some(after)
          else Some(before)
      }
    }

    def getAroundCount(index: Int, intArgs: IndexedSeq[Int], intPreds: IndexedSeq[Int]): Int = {
      val argWhereAndWhatOpt: Option[(Int, Int)] = findWhereAndWhatOptAround(intArgs, index)(_ != 0)
      val predWhereAndWhatOpt: Option[(Int, Int)] = findWhereAndWhatOptAround(intPreds, index)(_ != 0)

      if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined)
        indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
      else
        0
    }

    def getRightThenLeftCount(index: Int, intArgs: IndexedSeq[Int], intPreds: IndexedSeq[Int]): Int = {
      // Prefer looking to the right, after.
      val argWhereAndWhatOpt: Option[(Int, Int)] = Collection.findWhereAndWhatOptAfter(intArgs, index)(_ != 0)
      val predWhereAndWhatOpt: Option[(Int, Int)] = Collection.findWhereAndWhatOptAfter(intPreds, index)(_ != 0)
      if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined)
        indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
      else {
        // Then look left if necessary, before.
        val argWhereAndWhatOpt = Collection.findWhereAndWhatOptBefore(intArgs, index)(_ != 0)
        val predWhereAndWhatOpt = Collection.findWhereAndWhatOptBefore(intPreds, index)(_ != 0)
        if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined) {
          indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
        }
        else 0
      }
    }

    def findInexactPredicateGroundings(mentionRange: Range, sentenceHelper: SentenceHelper): Seq[PredicateGrounding] = {
      val mentionStart = mentionRange.start
      val isSkips: IndexedSeq[Boolean] = mentionRange.map(sentenceHelper.isSkipword)
      val indices: IndexedSeq[Int] = isSkips.indices // indices(0) + mentionStart = mentionRange(0)

      if (indices.forall(isSkips)) Seq.empty // There is nothing to ground, so short-circuit it.
      else {
        val propertyOntologyGroundingOpts: Seq[Option[OntologyGrounding]] = indices.map { index =>
          if (isSkips(index)) None
          else maybeProperty(Interval(mentionStart + index), sentenceHelper)
        }
        val isProps: IndexedSeq[Boolean] = indices.map { index => propertyOntologyGroundingOpts(index).isDefined }
        val isArgs: IndexedSeq[Boolean] = indices.map { index => !isSkips(index) && !isProps(index) && sentenceHelper.isArg(mentionStart + index) }
        val isPreds: IndexedSeq[Boolean] = indices.map { index => !isSkips(index) && !isProps(index) && !isArgs(index) }
        // Since these are all mutually exclusive, one could go through just once and trickle down.
        // Since isPreds is the catch-all, there must be something for every index.  There is no other.
        val intArgs: IndexedSeq[Int] = boolsToInts(isArgs) // turns (false, true, false, true) to (0, 1, 0 2)
        val intPreds: IndexedSeq[Int] = boolsToInts(isPreds).map(_ * -1) // turns (true, false, false, true) to (-1, 0, 0, -2)
        // 0, either not property or property that does not belong to arg or pred
        // +n, property that belongs to an arg
        // -n, property that belongs to a pred
        val intProps: IndexedSeq[Int] = indices.map { index =>
          if (!isProps(index)) 0
          else getRightThenLeftCount(index, intArgs, intPreds)
          // else getAroundCount(index, intArgs, intPreds)
        }
        // There is a property and it maps to no arg or pred.
        val zeroPropertyIndexes = indices.filter { index => isProps(index) && intProps(index) == 0 }
        val predicateTupleOpt =
          if (zeroPropertyIndexes.nonEmpty) {
            // This can only happen if there are no arguments or predicates.
            // This cannot be None because zeroProperties is nonEmpty.
            val propertyOntologyGroundingOpt = getBest(zeroPropertyIndexes, propertyOntologyGroundingOpts)
            // Arbitrarily put the property in the concept property slot.
            Some(PredicateTuple(None, propertyOntologyGroundingOpt, None, None, emptyOntologyGrounding))
          }
          else {
            val argOntologyGroundingOpt = Collection.optIndexOf(intArgs, 1) // Since we're using the first one only, or none.
                .flatMap { index =>
                  groundToBranchesOpt(Seq(SRLCompositionalGrounder.CONCEPT), Interval(mentionStart + index), s, topNOpt, thresholdOpt)
                }
            // We're only grounding against the very first argument or predicate, so +/- 1.
            val argPropertyIndexes = indices.filter { index => isProps(index) && intProps(index) == 1 }
            // If the argOntologyGroundingOpt didn't work out and is None, then the bestArgPropertiesOpt should also be None.
            val bestArgPropertiesOpt = argOntologyGroundingOpt.flatMap(_ => getBest(argPropertyIndexes, propertyOntologyGroundingOpts))

            val predOntologyGroundingOpt = Collection.optIndexOf(intPreds, -1) // Since we're using the first one only, or none.
                .flatMap { index =>
                  groundToBranchesOpt(Seq(SRLCompositionalGrounder.PROCESS), Interval(mentionStart + index), s, topNOpt, thresholdOpt)
                }
            // We're only grounding against the very first argument or predicate, so +/- 1.
            val predPropertyIndexes = indices.filter { index => isProps(index) && intProps(index) == -1 }
            // If the predOntologyGroundingOpt didn't work out and is None, then the bestPredPropertiesOpt should also be None.
            val bestPredPropertiesOpt = predOntologyGroundingOpt.flatMap(_ => getBest(predPropertyIndexes, propertyOntologyGroundingOpts))

            if (argOntologyGroundingOpt.isEmpty && predOntologyGroundingOpt.isEmpty) None // Don't even bother.
            else Some(PredicateTuple(argOntologyGroundingOpt, bestArgPropertiesOpt, predOntologyGroundingOpt, bestPredPropertiesOpt, emptyOntologyGrounding))
          }
        val predicateGroundings = predicateTupleOpt.toSeq.map(PredicateGrounding)
        val sortedPredicateGroundings = predicateGroundings.sortBy(-_.score)
        val slicedPredicateGroundings = topNOpt.map(sortedPredicateGroundings.take).getOrElse(sortedPredicateGroundings)
        slicedPredicateGroundings
      }
    }

    def findExactAndInexactPredicateGroundings(exactPredicateGroundingAndRangeAndBranch: (PredicateGrounding, Range, String), mentionRange: Range, sentenceHelper: SentenceHelper): Seq[PredicateGrounding] = {
      // The longest matches are in exactRange and could be multi-word.  Here we're just testing individual words.
      // The exactPredicateGrounding can have any branch in SRLCompositionalGrounder.processOrConceptBranches.
      // If an inexactPredicateGrounding conflicts in branch (actually slot) with the exactPredicateGrounding, only
      // the exact version is used when they are combined.
      val (exactPredicateGrounding, exactRange, exactBranch) = exactPredicateGroundingAndRangeAndBranch
      val exactOntologyGrounding =
          if (exactBranch == SRLCompositionalGrounder.CONCEPT) exactPredicateGrounding.predicateTuple.theme
          else exactPredicateGrounding.predicateTuple.themeProcess
      val exactIsArg = exactBranch == SRLCompositionalGrounder.CONCEPT // Args are concepts, Preds are processes.
      val exactAndInexactPredicateGroundings = mentionRange
          .filterNot(exactRange.contains)
          .filterNot(sentenceHelper.isSkipword)
          .flatMap { index =>
            val propertyOpt: Option[OntologyGrounding] = maybeProperty(Interval(index), sentenceHelper)
            val predicateTupleOpt =
                if (propertyOpt.isDefined) {
                  if (exactBranch == SRLCompositionalGrounder.CONCEPT)
                    Some(PredicateTuple(exactOntologyGrounding, propertyOpt.get, emptyOntologyGrounding, emptyOntologyGrounding, Set(index)))
                  else
                    Some(PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, exactOntologyGrounding, propertyOpt.get, Set(index)))
                }
                else {
                  val isArg = sentenceHelper.isArg(index)
                  if (isArg == exactIsArg) None // They cannot be combined.
                  else
                    if (isArg) {
                      val inexactOntologyGrounding = groundToBranches(Seq(SRLCompositionalGrounder.CONCEPT), Interval(index), s, topNOpt, thresholdOpt)
                      Some(PredicateTuple(inexactOntologyGrounding, emptyOntologyGrounding, exactOntologyGrounding, emptyOntologyGrounding, Set(index)))
                    }
                    else {
                      val inexactOntologyGrounding = groundToBranches(Seq(SRLCompositionalGrounder.PROCESS), Interval(index), s, topNOpt, thresholdOpt)
                      Some(PredicateTuple(exactOntologyGrounding, emptyOntologyGrounding, inexactOntologyGrounding, emptyOntologyGrounding, mentionRange.toSet))
                    }
                }

            predicateTupleOpt.map(PredicateGrounding)
          }
      val predicateGroundings = exactPredicateGrounding +: exactAndInexactPredicateGroundings
      val sortedPredicateGroundings = predicateGroundings.sortBy(-_.score)
      val slicedPredicateGroundings = topNOpt.map(sortedPredicateGroundings.take).getOrElse(sortedPredicateGroundings)

      slicedPredicateGroundings
    }

    def groundWithPredicates(sentenceHelper: SentenceHelper, predicates: Seq[Int]): Seq[PredicateGrounding] = {
      val mentionRange = Range(tokenInterval.start, tokenInterval.end)
      val mentionStrings = mentionRange.map(s.lemmas.get(_).toLowerCase).toArray // used to be words, now lemmas
      val predicateGroundings = {
        val exactPredicateGroundingsAndRangesAndBranches = findExactPredicateGroundingAndRanges(mentionStrings, mentionRange)

        if (exactPredicateGroundingsAndRangesAndBranches.isEmpty)
          findInexactPredicateGroundings(mentionRange, sentenceHelper)
        else
          exactPredicateGroundingsAndRangesAndBranches.flatMap(findExactAndInexactPredicateGroundings(_, mentionRange, sentenceHelper))
      }

      predicateGroundings.sortBy(-_.score)
    }

    val sentenceHelper = SentenceHelper(s, tokenInterval, exclude)
    val validPredicates = sentenceHelper.validPredicates.distinct.sorted // Duplicates can be returned!
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

  private def getIndividualGroundings(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): IndividualGroundings = {
    // It might not be good to filter stopwords now because they can be included in the examples, like "right to use" or "food for work".
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
    val filtered: IndividualGroundings =
        if (branches.length == 1) branchesGroundings
        else filterAndSlice(branchesGroundings, topN, threshold)

    filtered
  }

  private def groundToBranchesOpt(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): Option[OntologyGrounding] = {
    val individualGroundings = getIndividualGroundings(branches, span, s, topN, threshold)

    if (individualGroundings.isEmpty) None
    else Some(newOntologyGrounding(individualGroundings))
  }

  private def groundToBranches(branches: Seq[String], span: Interval, s: Sentence, topN: Option[Int], threshold: Option[Float]): OntologyGrounding = {
    val individualGroundings = getIndividualGroundings(branches, span, s, topN, threshold)
    newOntologyGrounding(individualGroundings)
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
  // This higher threshold was used when scores could substantially exceed 1.0.
  // It has been lowered, in part to ensure that shortages matches wm/property/shortage.
  // val propertyConfidenceThreshold: Float = 0.85f
  val propertyConfidenceThreshold: Float = 0.70f
}
