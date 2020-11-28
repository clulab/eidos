package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.components.ComponentsBuilder
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.refiners.EidosRefiner
import org.clulab.wm.eidos.refiners.FinderRefiner
import org.clulab.wm.eidos.refiners.OdinRefiner
import org.clulab.wm.eidos.refiners.DocumentRefiner
import org.clulab.wm.eidos.refiners.ProcessorRefiner
import org.clulab.wm.eidos.refiners.RefinerOptions
import org.slf4j.{Logger, LoggerFactory}

/**
 * A system for text processing and information extraction
 *
 * Eidos accepts text, uses the processors project (via a modified EidosProcessor) to tokenize and annotate
 * it into a Document, tasks an engine from the odin project to extract Mentions from it with the help of local
 * Actions and Finders (now called FinderRefiners), repeatedly refines the odin mentions with OdinRefiners,
 * converts them into EidosMentions, refines those with EidosRefiners, and finally incorporates them into an
 * AnnotatedDocument.  For good measure there are also ProcessorRefiners that modify the initial Document.
 *
 * OdinRefiners is a collection of functions that each refines (convert, transform, modifies) a Seq[Mention]
 * (odin Mentions) to another Option[Seq[Mention]] which will be fed into the next refiner.
 *
 * EidosRefiners do the same, but with EidosMentions, although they are hidden in an AnnotatedDocument:
 * AnnotatedDocument => Option[AnnotatedDocument].
 *
 * In the meantime there are also ProcessorRefiners which work on the Documents.  See the refiners package.
 *
 * The collections of refiners form a pipeline which can be configured at runtime or even be supplied to
 * Eidos from elsewhere.
 */
class EidosSystem(val components: EidosComponents) {
  // The "copy constructor" below will take cheap-to-update values from the config, but expensive
  // values from eidosSystem.components, if present  It is the new reload().
  def this(config: Config, eidosSystemOpt: Option[EidosSystem] = None) =
      this(new ComponentsBuilder(config, EidosSystem.PREFIX, eidosSystemOpt.map(_.components)).build())
  def this() = this(EidosSystem.defaultConfig)
  // Python now uses the default, empty constructor above, but the line below remains for documentation purposes.
  // def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val debug = true
  protected val useTimer = false

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document, metadata: Metadata = Metadata()): Document = {
    val annotateRefiners = DocumentRefiner.mkAnnotateRefiners(components, RefinerOptions.irrelevant, metadata)
    val annotatedDoc = DocumentRefiner.refine(annotateRefiners, doc, useTimer)

    annotatedDoc
  }

  // Annotate the text using a Processor and then populate lexicon labels.
  // If there is a document time involved, please place it in the metadata
  // and use one of the calls that takes it into account.
  def annotate(text: String, metadata: Metadata = Metadata()): Document = {
    val processorRefiner = ProcessorRefiner.mkRefiner(components, RefinerOptions.irrelevant)
    val doc = ProcessorRefiner.refine(processorRefiner, text, useTimer)

    val annotatedDoc = annotateDoc(doc, metadata)

    annotatedDoc
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  // This is a partial version, mostly legacy.
  def extractMentionsFrom(annotatedDoc: Document): Seq[Mention] = {
    val finderRefiners = FinderRefiner.mkRefiners(components)
    val odinMentions = FinderRefiner.refine(finderRefiners, annotatedDoc, useTimer)

    val odinRefiners = OdinRefiner.mkHeadOdinRefiners(components, RefinerOptions.irrelevant)
    val refinedMentions = OdinRefiner.refine(odinRefiners, odinMentions, useTimer)

    refinedMentions
  }

  // This could be used with more dynamically configured refiners, especially if made public.
  // Refining is where, e.g., grounding and filtering happens.
  protected def extractFromDoc(annotatedDoc: Document, documentRefiners: Seq[DocumentRefiner], finderRefiners: Seq[FinderRefiner], odinRefiners: Seq[OdinRefiner],
      eidosRefiners: Seq[EidosRefiner]): AnnotatedDocument = {
    val refinedDoc = DocumentRefiner.refine(documentRefiners, annotatedDoc, useTimer)
    val odinMentions = FinderRefiner.refine(finderRefiners, refinedDoc, useTimer)
    val refinedOdinMentions = OdinRefiner.refine(odinRefiners, odinMentions, useTimer)
    val annotatedDocument = AnnotatedDocument(annotatedDoc, refinedOdinMentions)
    val refinedAnnotatedDocument = EidosRefiner.refine(eidosRefiners, annotatedDocument, useTimer)

    refinedAnnotatedDocument
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(annotatedDoc: Document, options: RefinerOptions, metadata: Metadata): AnnotatedDocument = {
    val documentRefiners = DocumentRefiner.mkRefiners(components, options, metadata)
    val finderRefiners = FinderRefiner.mkRefiners(components)
    val odinRefiners = OdinRefiner.mkRefiners(components, options)
    val eidosRefiners = EidosRefiner.mkRefiners(components, options)

    extractFromDoc(annotatedDoc, documentRefiners, finderRefiners, odinRefiners, eidosRefiners)
  }

  // Legacy version
  def extractFromDoc(
    annotatedDoc: Document,
    cagRelevantOnly: Boolean = true,
    dctOpt: Option[DCT] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromDoc(annotatedDoc, RefinerOptions(cagRelevantOnly), Metadata(dctOpt, idOpt))
  }

  // MAIN PIPELINE METHOD if given text
  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument = {
    val annotatedDoc = annotate(text, metadata)

    extractFromDoc(annotatedDoc, options.refinerOptions, metadata)
  }

  // Legacy versions
  def extractFromText(
    text: String,
    cagRelevantOnly: Boolean = true,
    dctStringOpt: Option[String] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromText(text, EidosOptions(cagRelevantOnly), Metadata(this, dctStringOpt, idOpt))
  }

  def extractFromTextWithDct(
    text: String,
    cagRelevantOnly: Boolean = true,
    dctOpt: Option[DCT] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromText(text, EidosOptions(cagRelevantOnly), Metadata(dctOpt, idOpt))
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

  // Taxonomy relations that should make it to final causal analysis graph
  val CAUSAL_LABEL = "Causal"
  val CONCEPT_LABEL = "Concept"
  val CONCEPT_EXPANDED_LABEL = "Concept-Expanded"
  val CORR_LABEL = "Correlation"
  val COREF_LABEL = "Coreference"
  val MIGRATION_LABEL = "HumanMigration"
  // Taxonomy relations for other uses
  val RELATION_LABEL = "EntityLinker"

  // CAG filtering
  val CAG_EDGES: Set[String] = Set(CAUSAL_LABEL, CONCEPT_EXPANDED_LABEL, CORR_LABEL, COREF_LABEL)
  val EXPAND: Set[String] = CAG_EDGES ++ Set(MIGRATION_LABEL)

  def defaultConfig: Config = ConfigFactory.load("eidos")

  // Turn off warnings from this class.
  edu.stanford.nlp.ie.NumberNormalizer.setVerbose(false)
}
