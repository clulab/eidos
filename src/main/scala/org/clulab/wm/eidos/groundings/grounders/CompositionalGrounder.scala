package org.clulab.wm.eidos.groundings.grounders

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, ConceptPatterns, EidosWordToVec, OntologyGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.OdinUtils
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.ontologies.DomainOntology

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
    val property = newOntologyGrounding(strings.flatMap { string => nodesPatternMatched(string, conceptPatternsSeq(CompositionalGrounder.PROPERTY)) }, Some(CompositionalGrounder.PROPERTY))
    val process = newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddingsSeq(CompositionalGrounder.PROCESS)).map(SingleOntologyNodeGrounding(_)), Some(CompositionalGrounder.PROCESS))
    val concept = newOntologyGrounding(w2v.calculateSimilarities(strings, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT)).map(SingleOntologyNodeGrounding(_)), Some(CompositionalGrounder.CONCEPT))

    Seq(property, process, concept)
  }

  override def groundEidosMention(mention: EidosMention, topN: Option[Int] = None, threshold: Option[Float] = None): Seq[OntologyGrounding] = {
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

      val mentionText = mention.odinMention.words.toArray
      val allMentions = mentionHeadOpt.toSeq ++ modifierMentions
      val allMentionTokens = allMentions.flatMap(m=>m.words)

      // Get all groundings for each branch.
      // Each branch is its own grounding strategy to allow best performance.
      // The groundings of concepts and processes are competing at last, as it gains better performance.
      val propertySimilarities = allMentions.flatMap(m => nodesPatternMatched(m.text, conceptPatternsSeq(CompositionalGrounder.PROPERTY)))
      val processSimilarities = allMentions
        .flatMap(m => w2v.calculateSimilarities(m.words.toArray, conceptEmbeddingsSeq(CompositionalGrounder.PROCESS)))
        .map(SingleOntologyNodeGrounding(_))
      val conceptSimilarities = {
        val mentionHeadTagIsNN = mentionHeadOpt.map(m=>m.tags.head.head.startsWith("NN")).getOrElse(false)
        if (!mentionHeadTagIsNN) {
          if (modifierMentions.isEmpty) {
            val posTags = mention.odinMention.tags.getOrElse(Seq.empty)
            w2v.calculateSimilaritiesWeighted(mentionText, posTags, 5, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT))
              .map(SingleOntologyNodeGrounding(_))
          }
          else
          {
            val allMentionTags = allMentions.flatMap(mention =>mention.tags).flatten
            w2v.calculateSimilaritiesWeighted(allMentionTokens.toArray, allMentionTags, 1, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT))
              .map(SingleOntologyNodeGrounding(_))
          }
        }
        else
        {
          allMentions.flatMap(m => w2v.calculateSimilarities(m.words.toArray, conceptEmbeddingsSeq(CompositionalGrounder.CONCEPT)))
            .map(SingleOntologyNodeGrounding(_))
        }
      }

      // Start filtering procedure
      val effectiveThreshold = threshold.getOrElse(CompositionalGrounder.defaultThreshold)
      val effectiveTopN = topN.getOrElse(CompositionalGrounder.defaultGroundTopN)

      // Filtering procedure: let process and concept to compete with each other:
      def getTopKGrounding(similarities: Seq[SingleOntologyNodeGrounding], branch:String):OntologyGrounding = {
        val goodSimilarities = similarities
            .filter(_.score >= effectiveThreshold) // Filter these before sorting!
            .sortBy(-_.score)
            .take(effectiveTopN)
            .filter(_.namer.branch.get == branch)
        newOntologyGrounding(goodSimilarities, Some(branch))
      }
      val goodPropertyGroundings = getTopKGrounding(propertySimilarities, CompositionalGrounder.PROPERTY)
      val goodProcessGroundings = getTopKGrounding(processSimilarities++conceptSimilarities, CompositionalGrounder.PROCESS)
      val goodConceptGroundings = getTopKGrounding(processSimilarities++conceptSimilarities, CompositionalGrounder.CONCEPT)

      val goodGroundings = Seq(goodPropertyGroundings, goodProcessGroundings, goodConceptGroundings)

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
  val ENTITY = "entity"

  val branches: Seq[String] = Seq(PROCESS, PROPERTY, CONCEPT, ENTITY)

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
