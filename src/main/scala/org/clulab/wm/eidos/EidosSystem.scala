package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.document.PostProcessing
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.actions.MigrationHandler

import scala.annotation.tailrec

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val components: EidosComponents) {
  // The constructor below will take cheap to update values from the config, but expensive
  // values from eidosSystem.components, if present  It is the new reload().
  def this(config: Config, eidosSystemOpt: Option[EidosSystem] = None) =
      this(new EidosComponentsBuilder().add(config, eidosSystemOpt.map(_.components)).build())
  def this() = this(EidosSystem.defaultConfig)
  // Python now uses the default, empty constructor above, but the line below remains for documentation purposes.
  // def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val debug = true

  // Grounding is the first PostProcessing step(s) and it is pre-configured in Eidos.  Other things can take
  // the resulting AnnotatedDocument and post-process it further.  They are not yet integrated into Eidos.
  val postProcessors: Seq[PostProcessing] = Seq(components.ontologyHandler)

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document): Document = {
    // It is assumed and not verified that the document _has_not_ already been annotated.
    components.proc.annotate(doc)
    doc
  }

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String): Document = {
    val tokenized = components.proc.mkDocument(text, keepText = true) // Formerly keepText, must now be true
    val annotated = annotateDoc(tokenized)

    annotated
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  def extractFrom(doc: Document): Vector[Mention] = {

    def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
      val extractedEvents = components.engine.extractFrom(doc, state).toVector
      val mostCompleteEvents = components.actions.keepMostCompleteEvents(extractedEvents, State(extractedEvents)).toVector

      mostCompleteEvents
    }

    require(doc.text.isDefined)
    // Prepare the initial state -- if you are using the entity finder then it contains the found entities,
    // else it is empty
    val initialState = components.entityFinders.foldLeft(new State()) { (state, entityFinder) =>
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

  def postProcess(annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    val lastAnnotatedDocument = postProcessors.foldLeft(annotatedDocument) { (nextAnnotatedDocument, postProcessor) =>
      postProcessor.process(nextAnnotatedDocument)
    }

    lastAnnotatedDocument
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(
      doc: Document,
      cagRelevantOnly: Boolean = true,
      dctOpt: Option[DCT] = None,
      id: Option[String] = None): AnnotatedDocument = {
    // It is assumed and not verified that the document _has_ already been annotated.
    // Prepare the document here for further extraction.
    require(doc.text.isDefined)
    doc.id = id
    dctOpt.foreach { dct =>
      DctDocumentAttachment.setDct(doc, dct)
    }
    // Extract Mentions
    val odinMentions = extractFrom(doc)
    // Expand the Concepts that have a modified state if they are not part of a causal event
    val afterExpandingConcepts = components.conceptExpander.expand(odinMentions)
    val mentionsAndNestedArgs = {
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

      traverse(afterExpandingConcepts, Seq.empty, Set.empty)
    }
    //println(s"\nodinMentions() -- entities : \n\t${odinMentions.map(m => m.text).sorted.mkString("\n\t")}")
    val cagRelevant =
        if (cagRelevantOnly) components.stopwordManager.keepCAGRelevant(mentionsAndNestedArgs)
        else mentionsAndNestedArgs
    // TODO: handle hedging and negation...

    val afterHedging = components.hedgingHandler.detectHypotheses(cagRelevant, State(cagRelevant))
    val afterNegation = components.negationHandler.detectNegations(afterHedging)
    val afterMigration = components.migrationHandler.processMigrationEvents(afterNegation)
    val annotatedDocument = AnnotatedDocument(doc, afterMigration)

    postProcess(annotatedDocument)
  }

  def newDct(dctStringOpt: Option[String]): Option[DCT] = {
    val dctOpt = for (dctString <- dctStringOpt; timeNormFinder <- components.timeNormFinderOpt) yield {
      val dctOpt = timeNormFinder.parseDctString(dctString)
      if (dctOpt.isEmpty)
        EidosSystem.logger.warn(s"""The document creation time, "$dctString", could not be parsed.  Proceeding without...""")
      dctOpt
    }
    dctOpt.flatten
  }

  // MAIN PIPELINE METHOD if given text
  def extractFromText(
      text: String,
      cagRelevantOnly: Boolean = true,
      dctString: Option[String] = None,
      id: Option[String] = None): AnnotatedDocument = {
    extractFromTextWithDct(text, cagRelevantOnly, newDct(dctString), id)
  }

  def extractFromTextWithDct(
      text: String,
      cagRelevantOnly: Boolean = true,
      dct: Option[DCT] = None,
      id: Option[String] = None): AnnotatedDocument = {
    val document = annotate(text)
    extractFromDoc(document, cagRelevantOnly, dct, id)
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Helper Methods
  // ---------------------------------------------------------------------------------------------

  protected def debugPrint(str: String): Unit = if (debug) EidosSystem.logger.debug(str)

  protected def debugMentions(mentions: Seq[Mention]): Unit =
      mentions.foreach(m => debugPrint(s" * ${m.text} [${m.label}, ${m.tokenInterval}]"))
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

  // Turn off warnings from this class.
  edu.stanford.nlp.ie.NumberNormalizer.setVerbose(false)
}
