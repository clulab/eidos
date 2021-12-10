package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder.mkGrounder
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.TagSet
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.{EidosProcessor, EidosTokenizer, SentencesExtractor}
import org.clulab.wm.ontologies.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.ontologies.HalfTreeDomainOntology.HalfTreeDomainOntologyBuilder
import org.clulab.wm.ontologies.DomainOntology
import org.clulab.wm.ontologies.PosNegTreeDomainOntology.PosNegTreeDomainOntologyBuilder

class OntologyHandler(
  val ontologyGrounders: Seq[OntologyGrounder],
  val wordToVec: EidosWordToVec,
  val sentencesExtractor: SentencesExtractor,
  val canonicalizer: Canonicalizer,
  val includeParents: Boolean,
  val topN: Option[Int],
  val threshold: Option[Float]
) {

  def ground(eidosMention: EidosMention): EidosMention = {
    // If any of the grounders needs their own version, they'll have to make it themselves.
    val attachmentWords = eidosMention.odinMention.attachments.flatMap(a => EidosAttachment.getAttachmentWords(a))
    eidosMention.canonicalName = EidosMention.canonicalize(canonicalizer, eidosMention, attachmentWords)

    val ontologyGroundings = ontologyGrounders.flatMap { ontologyGrounder =>
      val name: String = ontologyGrounder.name
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundEidosMention(eidosMention, topN, threshold)
      val nameAndOntologyGroundings: Seq[(String, OntologyGrounding)] = ontologyGroundings.map { ontologyGrounding =>
        OntologyHandler.mkBranchName(name, ontologyGrounding.branchOpt) -> ontologyGrounding
      }

      nameAndOntologyGroundings
    }.toMap

    eidosMention.grounding = ontologyGroundings
    eidosMention
  }

  def reground(sentenceText: String, interval: Interval, document: Document): OntologyAliases.OntologyGroundingMap = {
    // This is assuming that there is just one sentence and the interval falls within it.  That may
    // not always be the case, especially as information may have originated with a different reader.
    // Furthermore, the interval may not align exactly with our tokenization of the sentence.  This
    // method expands the interval to use up entire tokens.  It is not valid to say that the interval
    // starts or stops on whitespace between tokens/words.

    def containsStart(interval: Interval, start: Int): Boolean =
      interval.start <= start && start < interval.end

    def containsEnd(interval: Interval, end: Int): Boolean =
    // This assumes non-empty intervals.  Otherwise one could have [0, 0), [0, n), etc.
    // Words generally cannot be empty, so this is a good bet.
      interval.start <= end && end <= interval.end

    try {
      val fullInterval = Interval(0, sentenceText.length) // Use entire length for exclusive end.
      require(interval.start <= interval.end)
      require(containsStart(fullInterval, interval.start))
      require(containsEnd(fullInterval, interval.end))

      val newDocument = document
      assert(newDocument.sentences.length >= 1)

      val sentence = newDocument.sentences.head
//      println("\nsentence:\t"+sentence.getSentenceText)
      val tokenIntervals = sentence.startOffsets.zip(sentence.endOffsets).map { case (start, end) => Interval(start, end) }

      val tokenStart = tokenIntervals.indexWhere { tokenInterval => containsStart(tokenInterval, interval.start) }
      assert(tokenStart >= 0)

      val tokenEnd = tokenIntervals.indexWhere { tokenInterval => containsEnd(tokenInterval, interval.end) }
      assert(tokenEnd >= 0)

      val tokenInterval = Interval(tokenStart, tokenEnd + 1) // Add one to make it exclusive.
      val odinMention = new TextBoundMention(EidosOntologyGrounder.GROUNDABLE, tokenInterval, sentence = 0, document, keep = true, foundBy = "OntologyHandler.reground")

      val annotatedDocument = AnnotatedDocument(newDocument, Seq(odinMention))
      assert(annotatedDocument.eidosMentions.size == 1)

      val eidosMention = annotatedDocument.eidosMentions.head

      ground(eidosMention)
      eidosMention.grounding
    }
    catch {
      case throwable: Throwable =>
        val ontologyGroundings: OntologyAliases.OntologyGroundingMap = Map.empty

        OntologyHandler.logger.error(s"Regrounding of '$sentenceText' on interval [${interval.start}-${interval.end}) was not possible ", throwable)
        ontologyGroundings
    }
  }

  def reground(sentenceText: String, interval: Interval): OntologyAliases.OntologyGroundingMap = {
    // This is assuming that there is just one sentence and the interval falls within it.  That may
    // not always be the case, especially as information may have originated with a different reader.
    // Furthermore, the interval may not align exactly with our tokenization of the sentence.  This
    // method expands the interval to use up entire tokens.  It is not valid to say that the interval
    // starts or stops on whitespace between tokens/words.

    def containsStart(interval: Interval, start: Int): Boolean =
        interval.start <= start && start < interval.end

    def containsEnd(interval: Interval, end: Int): Boolean =
        // This assumes non-empty intervals.  Otherwise one could have [0, 0), [0, n), etc.
        // Words generally cannot be empty, so this is a good bet.
        interval.start <= end && end <= interval.end

    try {
      val fullInterval = Interval(0, sentenceText.length) // Use entire length for exclusive end.
      require(interval.start <= interval.end)
      require(containsStart(fullInterval, interval.start))
      require(containsEnd(fullInterval, interval.end))

      val document = sentencesExtractor.extractDocument(sentenceText)
      assert(document.sentences.length == 1)

      val sentence = document.sentences.head
      val tokenIntervals = sentence.startOffsets.zip(sentence.endOffsets).map { case (start, end) => Interval(start, end) }

      val tokenStart = tokenIntervals.indexWhere { tokenInterval => containsStart(tokenInterval, interval.start) }
      assert(tokenStart >= 0)

      val tokenEnd = tokenIntervals.indexWhere { tokenInterval => containsEnd(tokenInterval, interval.end) }
      assert(tokenEnd >= 0)

      val tokenInterval = Interval(tokenStart, tokenEnd + 1) // Add one to make it exclusive.
      val odinMention = new TextBoundMention(EidosOntologyGrounder.GROUNDABLE, tokenInterval, sentence = 0, document, keep = true, foundBy = "OntologyHandler.reground")

      val annotatedDocument = AnnotatedDocument.apply(document, Seq(odinMention))
      assert(annotatedDocument.eidosMentions.size == 1)

      val eidosMention = annotatedDocument.eidosMentions.head

      ground(eidosMention)
      eidosMention.grounding
    }
    catch {
      case throwable: Throwable =>
        val ontologyGroundings: OntologyAliases.OntologyGroundingMap = Map.empty

        OntologyHandler.logger.error(s"Regrounding of '$sentenceText' on interval [${interval.start}-${interval.end}) was not possible ", throwable)
        ontologyGroundings
    }
  }

  def reground(name: String = "Custom", ontologyYaml: String, texts: Seq[String], filter: Boolean = true, topk: Int = 10, isAlreadyCanonicalized: Boolean = true): Array[Array[(String, Float)]] = {

    def reformat(grounding: OntologyGrounding): Array[(String, Float)] ={
      val topGroundings = grounding.take(topk).toArray
      topGroundings.map(gr => (gr.name, gr.score))
    }

    // FIXME: the original canonicalization needed the attachment words,
    //  here we would need to process the sentence further to get them...
    def recanonicalize(text: String): Seq[String] = {
      for {
        s <- sentencesExtractor.extractSentences(text)
        // FIXME: added a reminder here that we are NOT currently omitting attachment words in the regrounding!
        word <- canonicalizer.canonicalWordsFromSentence(s, Interval(0, s.words.length), excludedWords = Set())
      } yield word
    }

    //OntologyGrounding
    val ontology = OntologyHandler.mkDomainOntologyFromYaml(name, ontologyYaml, sentencesExtractor, canonicalizer, filter, includeParents)
    val grounder = EidosOntologyGrounder(name, ontology, wordToVec, canonicalizer)
    val groundings = grounder match {
      case g: EidosOntologyGrounder =>
        texts.toArray.map { text =>
          val mentionText =
              if (isAlreadyCanonicalized) text // It can't be restored, so make do.
              else text
          val canonicalNameParts =
              if (isAlreadyCanonicalized) text.split(' ')
              else recanonicalize(text).toArray // Attempt to regenerate them.

          g.groundText(mentionText, canonicalNameParts)
      }
      case _ => throw new RuntimeException("Regrounding needs an EidosOntologyGrounder")
    }

    groundings.map(reformat)
  }
}

object OntologyHandler extends Logging {

  def fromConfig(config: Config = EidosSystem.defaultConfig, tokenizer: EidosTokenizer): OntologyHandler = {
    val sentenceExtractor  = EidosProcessor("english", cutoff = 150)
    val tagSet = sentenceExtractor.getTagSet
    val stopwordManager = StopwordManager.fromConfig(config, tagSet)
    OntologyHandler.load(config[Config]("ontologies"), sentenceExtractor, stopwordManager, tagSet, tokenizer)
  }

  def load(config: Config, proc: SentencesExtractor, stopwordManager: StopwordManager, tagSet: TagSet, tokenizer: EidosTokenizer): OntologyHandler = {
    val canonicalizer = new Canonicalizer(stopwordManager, tagSet)
    val cacheDir: String = config[String]("cacheDir")
    val useCacheForOntologies: Boolean = config[Boolean]("useCacheForOntologies")
    val useCacheForW2V: Boolean = config[Boolean]("useCacheForW2V")
    val includeParents: Boolean = config[Boolean]("includeParents")
    val topN: Option[Int] = config.get[Int]("groundTopN")
    val threshold: Option[Float] = config.get[Double]("groundThreshold").map(_.toFloat)
    val eidosWordToVec: EidosWordToVec = {
      // This isn't intended to be (re)loadable.  This only happens once.
      OntologyHandler.logger.info("Loading W2V...")
      EidosWordToVec(
        config[Boolean]("useGrounding"),
        config[String]("wordToVecPath"),
        config[Int]("topKNodeGroundings"), //TODO: I don't think the W2V should be the one slicing these if our grounding API takes it as a param
        config[Double]("groundNegScoreThreshold").toFloat,
        config[Double]("groundPenalizeValue").toFloat,
        cacheDir,
        useCacheForW2V
      )
    }
    // Load enabled ontologies
    val ontologyHandler = eidosWordToVec match {
      case _: RealWordToVec =>
        val ontologyNames: List[String] = config[List[String]]("ontologies")
        // Base grounding steps, which aren't compositional
        val ontologyGrounders: Seq[OntologyGrounder] = ontologyNames.map { ontologyName =>
          val path: String = config[String](ontologyName)
          val domainOntology = DomainHandler.mkDomainOntology(ontologyName, path, proc, canonicalizer, cacheDir,
              useCacheForOntologies, includeParents)
          val grounder = mkGrounder(ontologyName, domainOntology, eidosWordToVec, canonicalizer, tokenizer)

          grounder
        }

        new OntologyHandler(ontologyGrounders, eidosWordToVec, proc, canonicalizer, includeParents, topN, threshold)
      case _: FakeWordToVec => new OntologyHandler(Seq.empty, eidosWordToVec, proc, canonicalizer, includeParents, topN, threshold)
     case _ => ???
    }

    ontologyHandler
  }

  def mkDomainOntologyFromYaml(name: String, ontologyYaml: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean = true, includeParents: Boolean): DomainOntology = {
    if (includeParents) {
//      new FullTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)
      new PosNegTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)
    }
    else
      new HalfTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)
  }

  def serializedPath(name: String, dir: String, includeParents: Boolean): String =
    if (includeParents) s"$dir/$name.fast.serialized"
    else s"$dir/$name.serialized"

  def mkBranchName(ontologyName: String, branchNameOpt: Option[String]): String = {
    ontologyName + branchNameOpt.map { branch => "/" + branch }.getOrElse("")
  }
}
