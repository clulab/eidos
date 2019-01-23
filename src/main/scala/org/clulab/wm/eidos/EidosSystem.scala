package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._

import com.typesafe.config.{Config, ConfigFactory}

import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.wm.eidos.actions.ExpansionHandler
import org.clulab.wm.eidos.attachments.{HypothesisHandler, NegationHandler}
import org.clulab.wm.eidos.context.GeoDisambiguateParser
import org.clulab.wm.eidos.document.{AnnotatedDocument, EidosDocument}
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._

import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = ConfigFactory.load("eidos")) {
  def this(x: Object) = this() // Dummy constructor crucial for Python integration

  val eidosConf: Config = config[Config]("EidosSystem")
  val language: String = eidosConf[String]("language")
  val proc: Processor = {
    EidosSystem.logger.info("Loading processor...")
    language match {
      case "english" => new FastNLPProcessor
      case "spanish" => new SpanishCluProcessor
      case "portuguese" => new PortugueseCluProcessor
    }
  }
  // Prunes sentences form the Documents to reduce noise/allow reasonable processing time
  val documentFilter = FilterByLength(proc, cutoff = 150)
  val debug = true // Allow external control with var if needed
  val wordToVec: EidosWordToVec = {
    // This isn't intended to be (re)loadable.  This only happens once.
    EidosSystem.logger.info("Loading W2V...")
    EidosWordToVec(
      LoadableAttributes.useW2V,
      LoadableAttributes.wordToVecPath,
      LoadableAttributes.topKNodeGroundings,
      LoadableAttributes.cacheDir,
      LoadableAttributes.useCache
    )
  }

  val stopwordManager = StopwordManager.fromConfig(config[Config]("filtering"))
  val canonicalizer = new Canonicalizer(stopwordManager)
  val ontologyHandler = new OntologyHandler(proc, wordToVec, canonicalizer)

  /**
    * The loadable aspect here applies to (most of) the files whose paths are specified in the config.  These
    * files can be reloaded.  It does not refer to the config itself, which is set when the EidosSystem is
    * constructed.  For example, the masterRules, actions, and ontologyGrounders are read anew in the apply method
    * from the same files used the previous time.  The file contents may have changed the since then, and the
    * new contents (e.g., rules) will be used, which is the purpose of the class.  The values for useW2V and useCache
    * will not have changed since initial construction of EidosSystem.  Note that word2Vec will not be reloaded,
    * since that is done once above.  It's not expected to change.
    */
  class LoadableAttributes(
    // These are the values which can be reloaded.  Query them for current assignments.
    val entityFinder: EidosEntityFinder,
    val domainParams: DomainParams,
    val adjectiveGrounder: AdjectiveGrounder,
    val actions: EidosActions,
    val engine: ExtractorEngine,
    val ner: LexiconNER,
    val hedgingHandler: HypothesisHandler,
    val negationHandler: NegationHandler,
    val expansionHandler: ExpansionHandler,
    val ontologyGrounders: Seq[EidosOntologyGrounder],
    val multiOntologyGrounder: MultiOntologyGrounding,
    val timenorm: Option[TemporalCharbasedParser],
    val geonorm: Option[GeoDisambiguateParser]
  )

  object LoadableAttributes {
    // Extraction
    val       masterRulesPath: String = eidosConf[String]("masterRulesPath")
    val      quantifierKBPath: String = eidosConf[String]("quantifierKBPath")
    val     domainParamKBPath: String = eidosConf[String]("domainParamKBPath")
    val        quantifierPath: String = eidosConf[String]("quantifierPath")
    val        propertiesPath: String = eidosConf[String]("propertiesPath")
    val       entityRulesPath: String = eidosConf[String]("entityRulesPath")
    val        avoidRulesPath: String = eidosConf[String]("avoidRulesPath")
    val          taxonomyPath: String = eidosConf[String]("taxonomyPath")
    // Filtering
    val       topKNodeGroundings: Int = eidosConf[Int]("topKNodeGroundings")
    // Hedging
    val           hedgingPath: String = eidosConf[String]("hedgingPath")
    val              cacheDir: String = eidosConf[String]("cacheDir")
    // These are needed to construct some of the loadable attributes even though it isn't a path itself.
    val               maxHops: Int = eidosConf[Int]("maxHops")
    val      wordToVecPath: String = eidosConf[String]("wordToVecPath")
    val  timeNormModelPath: String = eidosConf[String]("timeNormModelPath")
    val   geoNormModelPath: String = eidosConf[String]("geoNormModelPath")
    val    geoWord2IdxPath: String = eidosConf[String]("geoWord2IdxPath")
    val      geoLoc2IdPath: String = eidosConf[String]("geoLoc2IdPath")

    val            useW2V: Boolean = eidosConf[Boolean]("useW2V")
    val       useTimeNorm: Boolean = eidosConf[Boolean]("useTimeNorm")
    val        useGeoNorm: Boolean = eidosConf[Boolean]("useGeoNorm")
    val          useCache: Boolean = eidosConf[Boolean]("useCache")


    val hypothesisHandler = HypothesisHandler(hedgingPath)
    val negationHandler = NegationHandler(language)
    val expansionHandler = ExpansionHandler(language)
    // For use in creating the ontologies


    def apply(): LoadableAttributes = {
      // Odin rules and actions:
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath, expansionHandler)
      // Domain Ontologies:
      val ontologyGrounders =
          if (useW2V)
            ontologyHandler.ontologyGroundersFromConfig(config[Config]("ontologies"))
          else
            Seq.empty
      val multiOntologyGrounder = new MultiOntologyGrounder(ontologyGrounders)
      val timenorm: Option[TemporalCharbasedParser] =
          if (useTimeNorm)
            FileUtils.withResourceAsFile(timeNormModelPath) { file =>
              // Be sure to use fork := true in build.sbt when doing this so that the dll is not loaded twice.
              Some(new TemporalCharbasedParser(file.getAbsolutePath))
            }
          else
            None
      val geonorm: Option[GeoDisambiguateParser] =
          if (useGeoNorm)
            // Be sure to use fork := true in build.sbt when doing this so that the dll is not loaded twice.
            Some(new GeoDisambiguateParser(geoNormModelPath, geoWord2IdxPath, geoLoc2IdPath))
          else
            None

      new LoadableAttributes(
        EidosEntityFinder(entityRulesPath, avoidRulesPath, maxHops = maxHops),
        DomainParams(domainParamKBPath),
        EidosAdjectiveGrounder(quantifierKBPath),
        actions,
        ExtractorEngine(masterRules, actions, actions.globalAction), // ODIN component
        LexiconNER(Seq(quantifierPath, propertiesPath), caseInsensitiveMatching = true), //TODO: keep Quantifier...
        hypothesisHandler,
        negationHandler,
        expansionHandler,
        ontologyGrounders,
        multiOntologyGrounder,
        timenorm,
        geonorm
      )
    }
  }

  var loadableAttributes: LoadableAttributes = {
    EidosSystem.logger.info("Loading loadableAttributes...")
    LoadableAttributes()
  }

  def reload(): Unit = loadableAttributes = LoadableAttributes()

  def annotateDoc(document: Document, keepText: Boolean = true, documentCreationTime: Option[String] = None, filename: Option[String]= None): EidosDocument = {
    val doc = EidosDocument(document, keepText)
    // Add the tags from the lexicons we load
    doc.sentences.foreach(addLexiconNER)
    // Time and Location
    doc.parseDCT(loadableAttributes.timenorm, documentCreationTime)
    doc.parseTime(loadableAttributes.timenorm)
    doc.parseGeoNorm(loadableAttributes.geonorm)
    // Document ID
    doc.id = filename
    doc
  }

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String, keepText: Boolean = true, documentCreationTime: Option[String] = None, filename: Option[String]= None): EidosDocument = {
    // Syntactic pre-processing
    val tokenized = proc.mkDocument(text, keepText = true)  // Formerly keepText, must now be true
    val filtered = documentFilter.filter(tokenized)         // Filter noise from document
    val annotated = proc.annotate(filtered)
    val doc = annotateDoc(annotated, keepText, documentCreationTime, filename)
    doc
  }

  /**
    * This will use any non-O tags that are found to overwrite anything that proc has previously found.
    */
  protected def addLexiconNER(s: Sentence): Unit = {
    val eidosEntities = loadableAttributes.ner.find(s)
    // The Portuguese parser does not currently generate entities, so we want to create an empty list here for
    // further processing and filtering operations that expect to be able to query the entities
    if (s.entities.isEmpty)
      s.entities = Some(eidosEntities)
    else {
      val procEntities = s.entities.get

      eidosEntities.indices.foreach { index =>
        if (eidosEntities(index) != EidosSystem.NER_OUTSIDE)
          // Overwrite only if eidosEntities contains something interesting.
          procEntities(index) = eidosEntities(index)
      }
    }
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(doc: EidosDocument, keepText: Boolean = true, cagRelevantOnly: Boolean = true,
      documentCreationTime: Option[String] = None, filename: Option[String] = None): AnnotatedDocument = {
    val odinMentions = extractFrom(doc)

    // Dig in and get any Mentions that currently exist only as arguments, so that they get to be part of the state
    @tailrec
    def traverse(ms: Seq[Mention], results: Seq[Mention], seen: Set[Mention]): Seq[Mention] = {
      ms match {
        case Nil => results
        case m +: rest if !seen.contains(m) =>
          //DisplayUtils.shortDisplay(m)
          val args = m.arguments.values.flatten
          traverse(rest ++ args, m +: results, seen + m)
        case m +: rest => traverse(rest, results, seen)
      }
    }

    val mentionsAndNestedArgs = traverse(odinMentions, Seq.empty, Set.empty)
    //println(s"\nodinMentions() -- entities : \n\t${odinMentions.map(m => m.text).sorted.mkString("\n\t")}")
    val cagRelevant = if (cagRelevantOnly) stopwordManager.keepCAGRelevant(mentionsAndNestedArgs) else mentionsAndNestedArgs
    // TODO: handle hedging and negation...
    val afterHedging = loadableAttributes.hedgingHandler.detectHypotheses(cagRelevant, State(cagRelevant))
    val afterNegation = loadableAttributes.negationHandler.detectNegations(afterHedging)
    val eidosMentions = EidosMention.asEidosMentions(afterNegation, new Canonicalizer(stopwordManager), loadableAttributes.multiOntologyGrounder)

    AnnotatedDocument(doc, afterNegation, eidosMentions)
  }

  // MAIN PIPELINE METHOD if given text
  def extractFromText(text: String, keepText: Boolean = true, cagRelevantOnly: Boolean = true,
      documentCreationTime: Option[String] = None, filename: Option[String] = None): AnnotatedDocument = {
    val eidosDoc = annotate(text, keepText, documentCreationTime, filename)

    extractFromDoc(eidosDoc, keepText, cagRelevantOnly, documentCreationTime, filename)
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = loadableAttributes.engine.extractFrom(doc, state).toVector
    loadableAttributes.actions.keepMostCompleteEvents(res, State(res)).toVector
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    // get entities
    val entities: Seq[Mention] = loadableAttributes.entityFinder.extractAndFilter(doc).toVector

    val events = extractEventsFrom(doc, State(entities)).distinct
    // Note -- in main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
    // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports
    //val cagRelevant = keepCAGRelevant(events)

    events
  }



  /**
    * Wrapper for using w2v on some strings
    */
  def stringSimilarity(string1: String, string2: String): Float = wordToVec.stringSimilarity(string1, string2)

  /**
    * Debugging Methods
    */
  def debugPrint(str: String): Unit = if (debug) EidosSystem.logger.debug(str)

  def debugMentions(mentions: Seq[Mention]): Unit =
      mentions.foreach(m => debugPrint(s" * ${m.text} [${m.label}, ${m.tokenInterval}]"))
}

object EidosSystem {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val PREFIX = "EidosSystem"

  val EXPAND_SUFFIX = "expandParams"
  val SPLIT_SUFFIX = "splitAtCC"

  // Taxonomy relations that should make it to final causal analysis graph
  val CAUSAL_LABEL = "Causal"
  val CORR_LABEL = "Correlation"
  val COREF_LABEL = "Coreference"
  // Taxonomy relations for other uses
  val RELATION_LABEL = "EntityLinker"

  // Stateful Labels used by webapp
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val QUANT_LABEL_AFFIX = "-Quant"
  val NER_OUTSIDE = "O"
  // Provenance info for sameAs scoring
  val SAME_AS_METHOD = "simple-w2v"

  // CAG filtering
  val CAG_EDGES: Set[String] = Set(CAUSAL_LABEL, CORR_LABEL, COREF_LABEL)
}
