package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.timenorm.neural.TemporalNeuralParser
import org.clulab.wm.eidos.actions.ExpansionHandler
import org.clulab.wm.eidos.attachments.{HypothesisHandler, NegationHandler}
import org.clulab.wm.eidos.context.GeoDisambiguateParser
import org.clulab.wm.eidos.document.{AnnotatedDocument, EidosDocument}
import org.clulab.wm.eidos.entities.{EidosEntityFinder, EntityFinder}
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.timenorm.neural.TemporalNeuralParser
import org.clulab.wm.eidos.actions.ExpansionHandler
import org.clulab.wm.eidos.context.GeoDisambiguateParser
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.reflect.io

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = EidosSystem.defaultConfig) {
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
    val entityFinder: Option[EntityFinder],
    val actions: EidosActions,
    val engine: ExtractorEngine,
    val ner: Option[LexiconNER],
    val hedgingHandler: HypothesisHandler,
    val negationHandler: NegationHandler,
    val expansionHandler: ExpansionHandler,
    val ontologyGrounders: Seq[EidosOntologyGrounder],
    val multiOntologyGrounder: MultiOntologyGrounding,
    val timenorm: Option[TemporalNeuralParser],
    val geonorm: Option[GeoDisambiguateParser]
  )

  object LoadableAttributes {
    // Extraction
    val       masterRulesPath: String = eidosConf[String]("masterRulesPath")
    val          taxonomyPath: String = eidosConf[String]("taxonomyPath")
    // Filtering
    val       topKNodeGroundings: Int = eidosConf[Int]("topKNodeGroundings")
    // Hedging
    val           hedgingPath: String = eidosConf[String]("hedgingPath")
    val              cacheDir: String = eidosConf[String]("cacheDir")
    val      wordToVecPath: String = eidosConf[String]("wordToVecPath")
    val       useLexicons: Boolean = eidosConf[Boolean]("useLexicons")
    val   useEntityFinder: Boolean = eidosConf[Boolean]("useEntityFinder")
    val            useW2V: Boolean = eidosConf[Boolean]("useW2V")
    val       useTimeNorm: Boolean = eidosConf[Boolean]("useTimeNorm")
    val        useGeoNorm: Boolean = eidosConf[Boolean]("useGeoNorm")
    val          useCache: Boolean = eidosConf[Boolean]("useCache")


    val hypothesisHandler = HypothesisHandler(hedgingPath)
    val negationHandler = NegationHandler(language)
    val expansionHandler = ExpansionHandler(language) // todo - make more optional
    // For use in creating the ontologies


    def apply(): LoadableAttributes = {
      // Odin rules and actions:
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath, expansionHandler)

      // Entity Finders can be used to preload entities into the odin state, their use is optional.
      val entityFinder = if (useEntityFinder) {
        val entityFinderConfig = config[Config]("entityFinder")
        entityFinderConfig[String]("finderType") match {
          case "eidos" => Some(EidosEntityFinder.fromConfig(entityFinderConfig))
          case _ => throw new RuntimeException(s"Unexpected entity finder type")
        }
      } else None

      // LexiconNER files
      val lexiconNER = if(useLexicons) {
        val lexiconNERConfig = config[Config]("lexiconNER")
        val lexicons = lexiconNERConfig[List[String]]("lexicons")
        Some(LexiconNER(lexicons, caseInsensitiveMatching = true))
      } else None

      // Ontologies
      val ontologyGrounders =
          if (useW2V)
            ontologyHandler.ontologyGroundersFromConfig(config[Config]("ontologies"))
          else
            Seq.empty
      val multiOntologyGrounder = new MultiOntologyGrounder(ontologyGrounders)

      // Temporal Parsing
      val timenorm: Option[TemporalNeuralParser] = {
        if (!useTimeNorm) None
        else {
          // Be sure to use fork := true in build.sbt when doing this so that the dll is not loaded twice.
          val timeNorm = new TemporalNeuralParser()
          Some(timeNorm)
        }
      }

		
      // Geospatial Parsing
      val geonorm: Option[GeoDisambiguateParser] =
          if (useGeoNorm)
            // Be sure to use fork := true in build.sbt when doing this so that the dll is not loaded twice.
            Some(GeoDisambiguateParser.fromConfig(config[Config]("geoparser")))
          else
            None

      new LoadableAttributes(
        entityFinder,
        actions,
        ExtractorEngine(masterRules, actions, actions.globalAction), // ODIN component
        lexiconNER,
        hypothesisHandler,
        negationHandler,
        expansionHandler,
        ontologyGrounders,
        multiOntologyGrounder,  // todo: do we need this and ontologyGrounders?
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

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(document: Document, keepText: Boolean = true, documentCreationTime: Option[String] = None, filename: Option[String]= None): EidosDocument = {
    val doc = EidosDocument(document, keepText)
    // Add the tags from the lexicons we load
    doc.sentences.foreach(addLexiconNER)
    // Time and Location
    doc.parseTime(loadableAttributes.timenorm, documentCreationTime)
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
    // LexiconNER is an option
    for (ner <- loadableAttributes.ner) {
      val eidosEntities = ner.find(s)

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
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  // MAIN PIPELINE METHOD if given text
  def extractFromText(text: String, keepText: Boolean = true, cagRelevantOnly: Boolean = true,
                      documentCreationTime: Option[String] = None, filename: Option[String] = None): AnnotatedDocument = {
    val eidosDoc = annotate(text, keepText, documentCreationTime, filename)

    extractFromDoc(eidosDoc, keepText, cagRelevantOnly, documentCreationTime, filename)
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

  def extractFrom(doc: Document): Vector[Mention] = {
    // Prepare the initial state -- if you are using the entity finder then it contains the found entities,
    // else it is empty
    val initalState = loadableAttributes.entityFinder match {
      case None => new State()
      case Some(ef) => State(ef.extract(doc))
    }
    // Run the main extraction engine, pre-populated with the initial state
    val events = extractEventsFrom(doc, initalState).distinct
    // Note -- in main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
    // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports
    //val cagRelevant = keepCAGRelevant(events)

    events
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = loadableAttributes.engine.extractFrom(doc, state).toVector
    loadableAttributes.actions.keepMostCompleteEvents(res, State(res)).toVector
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Helper Methods
  // ---------------------------------------------------------------------------------------------

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

  def defaultConfig: Config = ConfigFactory.load("eidos")
}
