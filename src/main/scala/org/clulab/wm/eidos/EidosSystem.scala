package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.PostProcessing
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.document.AnnotatedDocument.PreProcessing
import org.clulab.wm.eidos.document.Metadata

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val components: EidosComponents) {
  // The constructor below will take cheap-to-update values from the config, but expensive
  // values from eidosSystem.components, if present  It is the new reload().
  def this(config: Config, eidosSystemOpt: Option[EidosSystem] = None) =
      this(new EidosComponentsBuilder().add(config, eidosSystemOpt.map(_.components)).build())
  def this() = this(EidosSystem.defaultConfig)
  // Python now uses the default, empty constructor above, but the line below remains for documentation purposes.
  // def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val debug = true

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document): Document = {
    // It is assumed and not verified that the document has _not_ already been annotated.
    components.proc.annotate(doc)
    doc
  }

  // Annotate the text using a Processor and then populate lexicon labels.
  def annotate(text: String): Document = {
    val tokenizedDoc = components.proc.mkDocument(text, keepText = true) // Formerly keepText, must now be true
    val annotatedDoc = annotateDoc(tokenizedDoc)

    annotatedDoc
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  protected def mkMentions(doc: Document): Seq[Mention] = {
    require(doc.text.isDefined)

    // Prepare the initial state.  If you are using the entity finder, then it
    // contains the found entities; otherwise, it is empty.
    val initialState = components.entityFinders.foldLeft(new State()) { (state, entityFinder) =>
      val mentions = entityFinder.find(doc, state)

      state.updated(mentions)
    }

    components.engine.extractFrom(doc, initialState)
  }

  // This abbreviated collection is used in a couple of apps that do not need the entire pipeline.
  // Note: In main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
  // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports.
  // val cagRelevant = keepCAGRelevant(events)
  protected val initialPreProcessors = Seq(
    (odinMentions: Seq[Mention]) => { components.actions.keepMostCompleteEvents(odinMentions) },
    (odinMentions: Seq[Mention]) => { odinMentions.distinct }
  )

  def extractMentionsFrom(doc: Document): Seq[Mention] = {
    val odinMentions = mkMentions(doc)

    preProcess(initialPreProcessors, odinMentions)
  }

  def preProcess(preProcessors: Seq[PreProcessing], odinMentions: Seq[Mention]): Seq[Mention] = {
    val lastOdinMentions = preProcessors.foldLeft(odinMentions) { (prevOdinMentions, preProcessor) =>
      val nextOdinMentions = preProcessor(prevOdinMentions)

      nextOdinMentions // inspect here
    }

    lastOdinMentions
  }

  def postProcess(postProcessors: Seq[PostProcessing], annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    val lastAnnotatedDocument = postProcessors.foldLeft(annotatedDocument) { (prevAnnotatedDocument, postProcessor) =>
      val nextAnnotatedDocument = postProcessor.postProcess(prevAnnotatedDocument)

      nextAnnotatedDocument // inspect here
    }

    lastAnnotatedDocument
  }

  // Grounding is the first PostProcessing step(s) and it is pre-configured in Eidos.  Other things can take
  // the resulting AnnotatedDocument and post-process it further.  They are not yet integrated into Eidos.
  protected def mkPostProcessors(options: EidosSystem.Options): Seq[PostProcessing] = Seq(
    components.ontologyHandler
  )

  protected def mkPreProcessors(options: EidosSystem.Options): Seq[PreProcessing] = {
    initialPreProcessors ++
    Seq(
      (odinMentions: Seq[Mention]) => { components.conceptExpander.expand(odinMentions) },
      (odinMentions: Seq[Mention]) => { components.nestedArgumentExpander.traverse(odinMentions) },
      (odinMentions: Seq[Mention]) => {
        // This exception is dependent on runtime options.
        if (options.cagRelevantOnly) components.stopwordManager.keepCAGRelevant(odinMentions)
        else odinMentions
      },
      (odinMentions: Seq[Mention]) => { components.hedgingHandler.detectHypotheses(odinMentions) },
      (odinMentions: Seq[Mention]) => { components.negationHandler.detectNegations(odinMentions) },
      (odinMentions: Seq[Mention]) => { components.migrationHandler.processMigrationEvents(odinMentions) }
    )
  }

  // This could be used with more dynamically configured processors, especially if made public.
  protected def extractFromDoc(doc: Document, preProcessors: Seq[PreProcessing], postProcessors: Seq[PostProcessing]): AnnotatedDocument = {
    val odinMentions = mkMentions(doc)
    val preProcessedMentions = preProcess(preProcessors, odinMentions)
    val annotatedDocument = AnnotatedDocument(doc, preProcessedMentions)
    val postProcessedDocument = postProcess(postProcessors, annotatedDocument)

    postProcessedDocument
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(doc: Document, options: EidosSystem.Options, metadata: Metadata): AnnotatedDocument = {
    // These could be organized in advance based on what we know about available options.
    val preProcessors = mkPreProcessors(options)
    val postProcessors = mkPostProcessors(options)

    metadata.attachToDoc(doc)
    extractFromDoc(doc, preProcessors, postProcessors)
  }

  // Legacy version
  def extractFromDoc(
    doc: Document,
    cagRelevantOnly: Boolean = true,
    dctOpt: Option[DCT] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromDoc(doc, EidosSystem.Options(cagRelevantOnly), Metadata(dctOpt, idOpt))
  }

  // MAIN PIPELINE METHOD if given text
  def extractFromText(text: String, options: EidosSystem.Options, metadata: Metadata): AnnotatedDocument = {
    val doc = annotate(text)

    extractFromDoc(doc, options, metadata)
  }

  // Legacy versions
  def extractFromText(
    text: String,
    cagRelevantOnly: Boolean = true,
    dctStringOpt: Option[String] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromText(text, EidosSystem.Options(cagRelevantOnly), Metadata(this, dctStringOpt, idOpt))
  }

  def extractFromTextWithDct(
    text: String,
    cagRelevantOnly: Boolean = true,
    dctOpt: Option[DCT] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromText(text, EidosSystem.Options(cagRelevantOnly), Metadata(dctOpt, idOpt))
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Helper Methods
  // ---------------------------------------------------------------------------------------------

  protected def debugPrint(message: String): Unit = if (debug) EidosSystem.logger.debug(message)

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

  class Options(val cagRelevantOnly: Boolean) {
  }

  object Options {

    def apply(cagRelevantOnly: Boolean): Options = new Options(cagRelevantOnly)
  }
}
