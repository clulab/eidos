package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigObject
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._
import org.clulab.wm.eidos.context.{GeoNormFinder, TimeNormFinder}
import org.clulab.wm.eidos.document.DctDocumentAttachment
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: EidosSystemConfig) {
  def this(config: Config = EidosSystem.defaultConfig) = this(new EidosSystemConfig(config))
  def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val geoNormFinderOpt: Option[GeoNormFinder] = config.entityFinders.collectFirst { case f: GeoNormFinder => f }
  val useGeoNorm: Boolean = geoNormFinderOpt.isDefined

  protected val timeNormFinderOpt: Option[TimeNormFinder] = config.entityFinders.collectFirst { case f: TimeNormFinder => f }
  val useTimeNorm: Boolean = timeNormFinderOpt.isDefined

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document): Document = {
    // It is assumed and not verified that the document _has_not_ already been annotated.
    config.proc.annotate(doc)
    doc
  }

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String): Document = {
    // Syntactic pre-processing
    val tokenized = config.proc.mkDocument(text, keepText = true)  // Formerly keepText, must now be true
    val filtered = config.documentFilter.filter(tokenized)         // Filter noise from document
    val annotated = annotateDoc(filtered)

    annotated
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  // MAIN PIPELINE METHOD if given text
  def extractFromText(
    text: String,
    cagRelevantOnly: Boolean = true,
    dctString: Option[String] = None,
    filename: Option[String] = None): AnnotatedDocument = {

    val document = annotate(text)
    extractFromDoc(document, cagRelevantOnly, dctString, filename)
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(
      doc: Document,
      cagRelevantOnly: Boolean = true,
      dctStringOpt: Option[String] = None,
      filename: Option[String] = None): AnnotatedDocument = {
    // It is assumed and not verified that the document _has_ already been annotated.
    require(doc.text.isDefined)

    doc.id = filename
    for (dctString <- dctStringOpt; timeNormFinder <- timeNormFinderOpt) {
      val dctOpt = timeNormFinder.parseDctString(dctString)

      dctOpt match {
        case Some(dct) =>
          DctDocumentAttachment.setDct(doc, dct)
        case None =>
          EidosSystem.logger.warn(s"""The document creation time, "$dctString", could not be parsed.  Proceeding without...""")
      }
    }

    // Extract Mentions
    val odinMentions = extractFrom(doc)
    // Expand the Concepts that have a modified state if they are not part of a causal event
    val afterExpandingConcepts = maybeExpandConcepts(odinMentions, config.keepStatefulConcepts)

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

    val mentionsAndNestedArgs = traverse(afterExpandingConcepts, Seq.empty, Set.empty)
    //println(s"\nodinMentions() -- entities : \n\t${odinMentions.map(m => m.text).sorted.mkString("\n\t")}")
    val cagRelevant = if (cagRelevantOnly) config.stopwordManager.keepCAGRelevant(mentionsAndNestedArgs) else mentionsAndNestedArgs
    // TODO: handle hedging and negation...
    val afterHedging = config.hedgingHandler.detectHypotheses(cagRelevant, State(cagRelevant))
    val afterNegation = config.negationHandler.detectNegations(afterHedging)
    val eidosMentions = EidosMention.asEidosMentions(afterNegation, new Canonicalizer(config.stopwordManager), config.multiOntologyGrounder)

    AnnotatedDocument(doc, afterNegation, eidosMentions)
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    require(doc.text.isDefined)
    // Prepare the initial state -- if you are using the entity finder then it contains the found entities,
    // else it is empty
    val initialState = config.entityFinders.foldLeft(new State()) { (state, entityFinder) =>
      val mentions = entityFinder.find(doc, state)
      state.updated(mentions)
    }

    // Run the main extraction engine, pre-populated with the initial state
    val events = extractEventsFrom(doc, initialState).distinct
    // Note -- in main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
    // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports
    //val cagRelevant = keepCAGRelevant(events)

    events
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val extractedEvents = config.engine.extractFrom(doc, state).toVector
    val mostCompleteEvents = config.actions.keepMostCompleteEvents(extractedEvents, State(extractedEvents)).toVector

    mostCompleteEvents
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Helper Methods
  // ---------------------------------------------------------------------------------------------

  /**
    * Wrapper for using w2v on some strings
    */
  def stringSimilarity(string1: String, string2: String): Float = config.ontologyHandler.wordToVec.stringSimilarity(string1, string2)

  /**
    * Debugging Methods
    */
  def debugPrint(str: String): Unit = if (config.debug) EidosSystem.logger.debug(str)

  def debugMentions(mentions: Seq[Mention]): Unit =
      mentions.foreach(m => debugPrint(s" * ${m.text} [${m.label}, ${m.tokenInterval}]"))

  // If enabled and applicable, expand Concepts which don't participate in primary events
  def maybeExpandConcepts(mentions: Seq[Mention], keepStatefulConcepts: Boolean): Seq[Mention] = {
    def isIncDecQuant(a: Attachment): Boolean = a.isInstanceOf[Increase] || a.isInstanceOf[Decrease] || a.isInstanceOf[Quantification]
    def expandIfNotExpanded(ms: Seq[Mention], expandedState: State): Seq[Mention] = {
      // Get only the Concepts that don't overlap with a previously expanded Concept...
      // todo: note this filter is based on token interval overlap, perhaps a smarter way is needed (e.g., checking the argument token intervals?)
      val notYetExpanded = ms.filter(m => expandedState.mentionsFor(m.sentence, m.tokenInterval).isEmpty)
      // Expand
      val expanded = config.expander.get.expand(notYetExpanded, new State())
      // Modify the label to flag them for keeping
      val relabeled = expanded.map(m => MentionUtils.withLabel(m, EidosSystem.CONCEPT_EXPANDED_LABEL))
      relabeled
    }

    // Check to see if we are keeping stateful concepts and if we have an expander
    if (!keepStatefulConcepts || config.expander.isEmpty) {
      mentions
    } else {
      // Split the mentions into Cpncepts and Relations by the label
      val (concepts, relations) = mentions.partition(_ matches EidosSystem.CONCEPT_LABEL)
      // Check to see if any of the Concepts have state attachments
      val (expandable, notExpandable) = concepts.partition(_.attachments.exists(isIncDecQuant))
      // Get the already expanded mentions for this document
      val prevExpandableState = State(relations.filter(rel => EidosSystem.CAG_EDGES.contains(rel.label)))
      // Expand the Concepts if they weren't already part of an expanded Relation
      val expandedConcepts = expandIfNotExpanded(expandable, prevExpandableState)
      expandedConcepts ++ notExpandable ++ relations
      }
    }
}

object EidosSystem {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val PREFIX = "EidosSystem"

  val EXPAND_SUFFIX = "expandParams"
  val SPLIT_SUFFIX = "splitAtCC"

  // Taxonomy relations that should make it to final causal analysis graph
  val CAUSAL_LABEL = "Causal"
  val CONCEPT_LABEL = "Concept"
  val CONCEPT_EXPANDED_LABEL = "Concept-Expanded"
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
  val CAG_EDGES: Set[String] = Set(CAUSAL_LABEL, CONCEPT_EXPANDED_LABEL, CORR_LABEL, COREF_LABEL)

  def defaultConfig: Config = ConfigFactory.load("eidos")
}


class EidosSystemConfig(protected val config: Config) {

  def getConfig(path: String): Config = config.getConfig(path)

  def root: ConfigObject = config.root

  val debug = true // Allow external control with var if needed
  EidosSystem.logger.info("Loading config...")

  protected val eidosConf: Config = config[Config]("EidosSystem")
  val language: String = eidosConf[String]("language")
  val proc: Processor = {
    EidosSystem.logger.info("Loading processor...")
    language match {
      case "english" => new FastNLPProcessor
      case "spanish" => new SpanishCluProcessor
      case "portuguese" => new PortugueseCluProcessor
    }
  }
  val negationHandler = NegationHandler(language)
  // Prunes sentences form the Documents to reduce noise/allow reasonable processing time
  val documentFilter = FilterByLength(proc, cutoff = 150)

  val stopwordManager: StopwordManager = StopwordManager.fromConfig(config)
  val ontologyHandler: OntologyHandler = OntologyHandler.load(config[Config]("ontologies"), proc, stopwordManager)

  val actions = EidosActions.fromConfig(config[Config]("actions"))
  val engine = { // ODIN component
    val masterRulesPath: String = eidosConf[String]("masterRulesPath")
    val masterRules = FileUtils.getTextFromResource(masterRulesPath)

    ExtractorEngine(masterRules, actions, actions.globalAction)
  }

  // This seems to be unused
  // protected val taxonomyPath: String = eidosConf[String]("taxonomyPath")

  // Hedging
  val hedgingHandler = {
    val hedgingPath: String = eidosConf[String]("hedgingPath")
    HypothesisHandler(hedgingPath)
  }

  // Entity Finders can be used to preload entities into the odin state, their use is optional.
  val entityFinders = Finder.fromConfig("EidosSystem.entityFinders", config)

  // Ontologies
  val multiOntologyGrounder = ontologyHandler.ontologyGrounders

  // Expander for expanding the bare events
  val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
  val expander = eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig)
  if (keepStatefulConcepts && expander.isEmpty)
    println("NOTICE: You're keeping stateful Concepts but didn't load an expander.")
}
