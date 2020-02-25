package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.document.{AnnotatedDocument, PostProcessing}
import org.clulab.wm.eidos.groundings.HalfTreeDomainOntology.HalfTreeDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.mkGrounder
import org.clulab.wm.eidos.groundings.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{Canonicalizer, StopwordManager}
import org.slf4j.{Logger, LoggerFactory}

class OntologyHandler(
  val ontologyGrounders: Seq[OntologyGrounder],
  val wordToVec: EidosWordToVec,
  val sentencesExtractor: SentencesExtractor,
  val canonicalizer: Canonicalizer,
  val includeParents: Boolean
) extends PostProcessing {

  protected def process(eidosMention: EidosMention): Unit = {
    // If any of the grounders needs their own version, they'll have to make it themselves.
    eidosMention.canonicalName = canonicalizer.canonicalize(eidosMention)

    val ontologyGroundings = ontologyGrounders.flatMap { ontologyGrounder =>
      val name: String = ontologyGrounder.name
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundOntology(eidosMention, topN = Option(5), threshold= Option(0.5f))
      val nameAndOntologyGroundings: Seq[(String, OntologyGrounding)] = ontologyGroundings.map { ontologyGrounding =>
        OntologyHandler.mkBranchName(name, ontologyGrounding.branch) -> ontologyGrounding
      }

      nameAndOntologyGroundings
    }.toMap

    eidosMention.grounding = ontologyGroundings
  }

  def process(annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    annotatedDocument.allEidosMentions.foreach { eidosMention =>
      process(eidosMention)
    }
    annotatedDocument
  }


  def reground(sentenceText: String, interval: Interval): OntologyAliases.OntologyGroundings = {
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

      val eidosMentions = EidosMention.asEidosMentions(Seq(odinMention))
      assert(eidosMentions.size == 1)

      val eidosMention = eidosMentions.head

      process(eidosMention)
      eidosMention.grounding
    }
    catch {
      case throwable: Throwable =>
        val ontologyGroundings: OntologyAliases.OntologyGroundings = Map.empty

        OntologyHandler.logger.error(s"Regrounding of '$sentenceText' on interval [${interval.start}-${interval.end}) was not possible ", throwable)
        ontologyGroundings
    }
  }

  def reground(name: String = "Custom", ontologyYaml: String, texts: Seq[String], filter: Boolean = true, topk: Int = 10, isAlreadyCanonicalized: Boolean = true): Array[Array[(String, Float)]] = {

    def reformat(grounding: OntologyGrounding): Array[(String, Float)] ={
      val topGroundings = grounding.take(topk).toArray
      topGroundings.map(gr => (gr._1.name, gr._2))
    }

    def recanonicalize(text: String): Seq[String] = {
      val sentences = sentencesExtractor.extractSentences(text)

      val contentLemmas = for {
        s <- sentences
        lemmas = s.lemmas.get
        ners = s.entities.get
        tags = s.tags.get
        i <- lemmas.indices
        if canonicalizer.isCanonical(lemmas(i), tags(i), ners(i))
      } yield lemmas(i)

      if (contentLemmas.isEmpty)
        sentences.flatMap(_.words)   // fixme -- better and cleaner backoff, to match what is done with a mention
      else
        contentLemmas
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

          g.groundOntology(isGroundableType = true, mentionText, canonicalNameParts)
      }
      case _ => throw new RuntimeException("Regrounding needs an EidosOntologyGrounder")
    }

    groundings.map(reformat)
  }
}

object OntologyHandler {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def load(config: Config, proc: SentencesExtractor, stopwordManager: StopwordManager): OntologyHandler = {
    val canonicalizer = new Canonicalizer(stopwordManager)
    val cacheDir: String = config[String]("cacheDir")
    val useCached: Boolean = config[Boolean]("useCache")
    val includeParents: Boolean = config[Boolean]("includeParents")
    val eidosWordToVec: EidosWordToVec = {
      // This isn't intended to be (re)loadable.  This only happens once.
      OntologyHandler.logger.info("Loading W2V...")
      EidosWordToVec(
        config[Boolean]("useW2V"),
        config[String]("wordToVecPath"),
        config[Int]("topKNodeGroundings"),
        cacheDir,
        useCached
      )
    }
    // Load enabled ontologies
    val ontologyHandler = eidosWordToVec match {
      case _: RealWordToVec =>
        val ontologyNames: List[String] = config[List[String]]("ontologies")
        // Base grounding steps, which aren't compositional
        val ontologyGrounders: Seq[OntologyGrounder] = ontologyNames.map { ontologyName =>
          val path: String = config[String](ontologyName)
          val domainOntology = DomainOntologies.mkDomainOntology(ontologyName, path, proc, canonicalizer, cacheDir,
              useCached, includeParents)
          val grounder = mkGrounder(ontologyName, domainOntology, eidosWordToVec, canonicalizer)

          grounder
        }

        new OntologyHandler(ontologyGrounders, eidosWordToVec, proc, canonicalizer, includeParents)
      case _: FakeWordToVec => new OntologyHandler(Seq.empty, eidosWordToVec, proc, canonicalizer, includeParents)
     case _ => ???
    }

    ontologyHandler
  }

  def mkDomainOntologyFromYaml(name: String, ontologyYaml: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean = true, includeParents: Boolean): DomainOntology = {
    if (includeParents)
      new FullTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)
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
