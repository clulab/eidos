package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.eidos.attachments.{HypothesisHandler, NegationHandler}
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.{FAO_NAMESPACE, INT_NAMESPACE, MESH_NAMESPACE, MITRE12_NAMESPACE, PROPS_NAMESPACE, UN_NAMESPACE, WDI_NAMESPACE, WHO_NAMESPACE}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._
import ai.lum.common.ConfigUtils._
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.wm.eidos.actions.ExpansionHandler
import org.clulab.wm.eidos.context.GeoDisambiguateParser

import scala.annotation.tailrec

case class AnnotatedDocument(document: Document, odinMentions: Seq[Mention], eidosMentions: Seq[EidosMention])

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
    val stopwordManager: StopwordManager,
    val hedgingHandler: HypothesisHandler,
    val negationHandler: NegationHandler,
    val expansionHandler: ExpansionHandler,
    val ontologyGrounders: Seq[EidosOntologyGrounder],
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
    val         stopwordsPath: String = eidosConf[String]("stopWordsPath")
    val       transparentPath: String = eidosConf[String]("transparentPath")
    // Hedging
    val           hedgingPath: String = eidosConf[String]("hedgingPath")
    // Ontology handling
    val        unOntologyPath: String = eidosConf[String]("unOntologyPath")
    val       wdiOntologyPath: String = eidosConf[String]("wdiOntologyPath")
    val       faoOntologyPath: String = eidosConf[String]("faoOntologyPath")
    val      meshOntologyPath: String = eidosConf[String]("meshOntologyPath")
    val     propsOntologyPath: String = eidosConf[String]("propsOntologyPath")
    val   mitre12OntologyPath: String = eidosConf[String]("mitre12OntologyPath")
    val       whoOntologyPath: String = eidosConf[String]("whoOntologyPath")
    val       intOntologyPath: String = eidosConf[String]("intOntologyPath")
    val              cacheDir: String = eidosConf[String]("cacheDir")

    // These are needed to construct some of the loadable attributes even though it isn't a path itself.
    val    ontologies: Seq[String] = eidosConf[List[String]]("ontologies")
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

    val stopwordManager = StopwordManager(stopwordsPath, transparentPath)
    val canonicalizer = new Canonicalizer(stopwordManager)
    val hypothesisHandler = HypothesisHandler(hedgingPath)
    val negationHandler = NegationHandler(language)
    val expansionHandler = ExpansionHandler(language)

    def mkDomainOntology(name: String, useCached: Boolean): DomainOntology = {
      val serializedPath: String = DomainOntologies.serializedPath(name, cacheDir)

      name match {
        case           UN_NAMESPACE =>         UNOntology(     unOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case          WDI_NAMESPACE =>        WDIOntology(    wdiOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case          FAO_NAMESPACE =>        FAOOntology(    faoOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case         MESH_NAMESPACE =>       MeshOntology(   meshOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case        PROPS_NAMESPACE => PropertiesOntology(  propsOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case      MITRE12_NAMESPACE =>    MITRE12Ontology(mitre12OntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case          WHO_NAMESPACE =>        WHOOntology(    whoOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case          INT_NAMESPACE =>        IntOntology(    intOntologyPath, serializedPath, proc, canonicalizer, useCache = useCached)
        case _ => throw new IllegalArgumentException("Ontology " + name + " is not recognized.")
      }
    }

    def apply(): LoadableAttributes = {
      // Odin rules and actions:
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath, expansionHandler)
      // Domain Ontologies:
      val ontologyGrounders =
          if (useW2V)
            ontologies.par.map(ontology => EidosOntologyGrounder(ontology, mkDomainOntology(ontology, useCache), wordToVec)).seq
          else
            Seq.empty
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
//        ExtractorEngine(masterRules, actions, actions.mergeAttachments), // ODIN component
        ExtractorEngine(masterRules, actions, actions.globalAction), // ODIN component
        LexiconNER(Seq(quantifierPath, propertiesPath), caseInsensitiveMatching = true), //TODO: keep Quantifier...
        stopwordManager,
        hypothesisHandler,
        negationHandler,
        expansionHandler,
        ontologyGrounders,
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

  object MultiOntologyGrounder extends MultiOntologyGrounder {

    def groundOntology(mention: EidosMention): Groundings = {
      // Some plugin grounders need to be run after the primary grounders, i.e., they depend on the output of the primary grounders
      val (primaryGrounders, secondaryGrounders) = loadableAttributes.ontologyGrounders.partition(_.isPrimary)

      val primaryGroundings = primaryGrounders.map (ontologyGrounder =>
        (ontologyGrounder.name, ontologyGrounder.groundOntology(mention))).toMap

      val secondaryGroundings = secondaryGrounders.map (ontologyGrounder =>
        (ontologyGrounder.name, ontologyGrounder.groundOntology(mention, primaryGroundings))).toMap

      primaryGroundings ++ secondaryGroundings
    }
  }

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
    val cagRelevant = if (cagRelevantOnly) keepCAGRelevant(mentionsAndNestedArgs) else mentionsAndNestedArgs
    // TODO: handle hedging and negation...
    val afterHedging = loadableAttributes.hedgingHandler.detectHypotheses(cagRelevant, State(cagRelevant))
    val afterNegation = loadableAttributes.negationHandler.detectNegations(afterHedging)
    val eidosMentions = EidosMention.asEidosMentions(afterNegation, new Canonicalizer(loadableAttributes.stopwordManager), this.MultiOntologyGrounder)

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
//    println("Entities that made it to EidosSystem:")
//    entities.foreach(e => DisplayUtils.shortDisplay(e))
    // filter entities which are entirely stop or transparent
    //println(s"In extractFrom() -- entities : \n\t${entities.map(m => m.text).sorted.mkString("\n\t")}")
    // Becky says not to filter yet
    //val filtered = loadableAttributes.ontologyGrounder.filterStopTransparent(entities)
    val filtered = entities
    //println(s"\nAfter filterStopTransparent() -- entities : \n\t${filtered.map(m => m.text).sorted.mkString("\n\t")}")

    val events = extractEventsFrom(doc, State(filtered)).distinct
    // Note -- in main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
    // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports
    //val cagRelevant = keepCAGRelevant(events)

    events
    //cagRelevant.toVector
  }

  // Old version
  def oldKeepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {
    // 1) These will be "Causal" and "Correlation" which fall under "Event"
    val cagEdgeMentions = mentions.filter(m => EidosSystem.CAG_EDGES.contains(m.label))
    // 2) and these will be "Entity", without overlap from above.
    val entityMentions = mentions.filter(m => m.matches("Entity") && m.attachments.nonEmpty)
    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val argumentMentions: Seq[Mention] = cagEdgeMentions.flatMap(_.arguments.values.flatten)
    // Put them all together.
    val goodMentions = cagEdgeMentions ++ entityMentions ++ argumentMentions
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    val relevantMentions = mentions.filter(m => goodMentions.exists(m.eq))

    relevantMentions
  }

  // New version
  def keepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {

    // 1) These will be "Causal" and "Correlation" which fall under "Event" if they have content
    val allMentions = State(mentions)
    val cagEdgeMentions = mentions.filter(m => releventEdge(m, allMentions))

    // Should these be included as well?

    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val cagEdgeArguments = cagEdgeMentions.flatMap(mention => mention.arguments.values.flatten.toSeq)
    // Put them all together.
    // val releventEdgesAndTheirArgs = cagEdgeMentions ++ cagEdgeArguments
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    mentions.filter(mention => isCAGRelevant(mention, cagEdgeMentions, cagEdgeArguments))
  }

  def isCAGRelevant(mention: Mention, cagEdgeMentions: Seq[Mention], cagEdgeArguments: Seq[Mention]): Boolean =
    // We're no longer keeping all modified entities
    //(mention.matches("Entity") && mention.attachments.nonEmpty) ||
      cagEdgeMentions.contains(mention) ||
      cagEdgeArguments.contains(mention)

  def releventEdge(m: Mention, state: State): Boolean = {
    m match {
      case tb: TextBoundMention => EidosSystem.CAG_EDGES.contains(tb.label)
      case rm: RelationMention => EidosSystem.CAG_EDGES.contains(rm.label)
      case em: EventMention => EidosSystem.CAG_EDGES.contains(em.label) && argumentsHaveContent(em, state)
      case cs: CrossSentenceMention => EidosSystem.CAG_EDGES.contains(cs.label)
      case _ => throw new UnsupportedClassVersionError()
    }
  }

  def argumentsHaveContent(mention: EventMention, state: State): Boolean = {
    val causes: Seq[Mention] = mention.arguments.getOrElse("cause", Seq.empty)
    val effects: Seq[Mention] = mention.arguments.getOrElse("effect", Seq.empty)

    if (causes.nonEmpty && effects.nonEmpty) // If it's something interesting,
    // then both causes and effects should have some content
      causes.exists(loadableAttributes.stopwordManager.hasContent(_, state)) && effects.exists(loadableAttributes.stopwordManager.hasContent(_, state))
    else
      true
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
  type Corpus = Seq[AnnotatedDocument]

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
