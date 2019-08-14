package org.clulab.wm.eidos

import java.time.LocalDateTime

import ai.lum.common.ConfigUtils._
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
import org.clulab.wm.eidos.actions.MigrationUtils.processMigrationEvents

import scala.annotation.tailrec

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
  val stopwordManager: StopwordManager = StopwordManager.fromConfig(config)
  val ontologyHandler: OntologyHandler = OntologyHandler.load(config[Config]("ontologies"), proc, stopwordManager)

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
    val entityFinders: Seq[Finder],
    val actions: EidosActions,
    val engine: ExtractorEngine,
    val hedgingHandler: HypothesisHandler,
    val negationHandler: NegationHandler,

    val multiOntologyGrounder: MultiOntologyGrounding,
    val expander: Option[Expander],
    val keepStatefulConcepts: Boolean
  )

  object LoadableAttributes {
    // Extraction
    val      masterRulesPath: String = eidosConf[String]("masterRulesPath")
    val         taxonomyPath: String = eidosConf[String]("taxonomyPath")
    // Hedging
    val          hedgingPath: String = eidosConf[String]("hedgingPath")
    val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")

    val hypothesisHandler = HypothesisHandler(hedgingPath)
    val negationHandler = NegationHandler(language)
    // For use in creating the ontologies

    def apply(): LoadableAttributes = {
      // Odin rules and actions:
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions.fromConfig(config[Config]("actions"))

      // Entity Finders can be used to preload entities into the odin state, their use is optional.
      val entityFinders = Finder.fromConfig("EidosSystem.entityFinders", config)

      // Ontologies
      val multiOntologyGrounder = ontologyHandler.ontologyGrounders

      // Expander for expanding the bare events
      val expander = eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig)
      if (keepStatefulConcepts && expander.isEmpty) println("NOTICE: You're keeping stateful Concepts but didn't load an expander.")

      new LoadableAttributes(
        entityFinders,
        actions,
        ExtractorEngine(masterRules, actions, actions.globalAction), // ODIN component
        hypothesisHandler,
        negationHandler,
        multiOntologyGrounder,  // todo: do we need this and ontologyGrounders?
        expander,
        keepStatefulConcepts
      )
    }
  }

  var loadableAttributes: LoadableAttributes = {
    EidosSystem.logger.info("Loading loadableAttributes...")
    LoadableAttributes()
  }

  protected def timeNormFinderOpt: Option[TimeNormFinder] = loadableAttributes.entityFinders.collectFirst{ case f: TimeNormFinder => f }
  def useGeoNorm: Boolean = loadableAttributes.entityFinders.collectFirst{ case f: GeoNormFinder => f }.isDefined
  def useTimeNorm: Boolean = timeNormFinderOpt.isDefined

  def reload(): Unit = loadableAttributes = LoadableAttributes()

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document): Document = {
    // It is assumed and not verified that the document _has_not_ already been annotated.
    proc.annotate(doc)
    doc
  }

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String): Document = {
    // Syntactic pre-processing
    val tokenized = proc.mkDocument(text, keepText = true)  // Formerly keepText, must now be true
    val filtered = documentFilter.filter(tokenized)         // Filter noise from document
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
    dctString: Option[String] = EidosSystem.defaultDctStringOpt,
    filename: Option[String] = None): AnnotatedDocument = {

    val document = annotate(text)
    extractFromDoc(document, cagRelevantOnly, dctString, filename)
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(
      doc: Document,
      cagRelevantOnly: Boolean = true,
      dctStringOpt: Option[String] = EidosSystem.defaultDctStringOpt,
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
          EidosSystem.logger.warn(s"""The document creation time, "${dctString}", could not be parsed.  Proceeding without...""")
      }
    }

    // Extract Mentions
    val odinMentions = extractFrom(doc)
    // Expand the Concepts that have a modified state if they are not part of a causal event
    val afterExpandingConcepts = maybeExpandConcepts(odinMentions, loadableAttributes.keepStatefulConcepts)

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
    val cagRelevant = if (cagRelevantOnly) stopwordManager.keepCAGRelevant(mentionsAndNestedArgs) else mentionsAndNestedArgs
    // TODO: handle hedging and negation...
    val afterHedging = loadableAttributes.hedgingHandler.detectHypotheses(cagRelevant, State(cagRelevant))
    val afterNegation = loadableAttributes.negationHandler.detectNegations(afterHedging)
    val afterMigrationProc = processMigrationEvents(afterNegation)
    val eidosMentions = EidosMention.asEidosMentions(afterMigrationProc, new Canonicalizer((stopwordManager)), loadableAttributes.multiOntologyGrounder)


    AnnotatedDocument(doc, afterNegation, eidosMentions)
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    require(doc.text.isDefined)
    // Prepare the initial state -- if you are using the entity finder then it contains the found entities,
    // else it is empty
    val initialState = loadableAttributes.entityFinders.foldLeft(new State()) { (state, entityFinder) =>
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
    val extractedEvents = loadableAttributes.engine.extractFrom(doc, state).toVector
    val mostCompleteEvents = loadableAttributes.actions.keepMostCompleteEvents(extractedEvents, State(extractedEvents)).toVector

    mostCompleteEvents
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Helper Methods
  // ---------------------------------------------------------------------------------------------

  /**
    * Wrapper for using w2v on some strings
    */
  def stringSimilarity(string1: String, string2: String): Float = ontologyHandler.wordToVec.stringSimilarity(string1, string2)

  /**
    * Debugging Methods
    */
  def debugPrint(str: String): Unit = if (debug) EidosSystem.logger.debug(str)

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
      val expanded = loadableAttributes.expander.get.expand(notYetExpanded, new State())
      // Modify the label to flag them for keeping
      val relabeled = expanded.map(m => MentionUtils.withLabel(m, EidosSystem.CONCEPT_EXPANDED_LABEL))
      relabeled
    }

    // Check to see if we are keeping stateful concepts and if we have an expander
    if (!keepStatefulConcepts || loadableAttributes.expander.isEmpty) {
      mentions
    } else {
      // Split the mentions into Cpncepts and Relations by the label
      val (concepts, relations) = mentions.partition(_ matches EidosSystem.CONCEPT_LABEL)
      // Check to see if any of the Concepts have state attachments
      val (expandable, notExpandable) = concepts.partition(_.attachments.exists(isIncDecQuant))
      // Get the already expanded mentions for this document
      val prevExpandableState = State(relations.filter(rel => EidosSystem.EXPAND.contains(rel.label)))
      // Expand the Concepts if they weren't already part of an expanded Relation
      val expandedConcepts = expandIfNotExpanded(expandable, prevExpandableState)
      expandedConcepts ++ notExpandable ++ relations
      }
    }
}

object EidosSystem {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val PREFIX = "EidosSystem"

  val EXPAND_SUFFIX = "expandParams"
  val SPLIT_SUFFIX = "splitAtCC"

  // Taxonomy relations that should make it to final causal analysis graph
  val CAUSAL_LABEL = "Causal"
  val CONCEPT_LABEL = "Concept"
  val CONCEPT_EXPANDED_LABEL = "Concept-Expanded"
  val CORR_LABEL = "Correlation"
  val COREF_LABEL = "Coreference"
  val MIGRATION_LABEL = "HumanMigration"
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
  val EXPAND: Set[String] = CAG_EDGES ++ Set(MIGRATION_LABEL)

  def defaultConfig: Config = ConfigFactory.load("eidos")

  def defaultDctStringOpt: Option[String] = Some(LocalDateTime.now.toString.substring(0, 10) + ".") // None
}
