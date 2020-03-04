package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.mentions.EidosMention

/**
 * A system for text processing and information extraction
 *
 * Eidos accepts text, uses the processors project (via a modified EidosProcessor) to tokenize and annotate
 * it into a Document, tasks an engine from the odin project to extract Mentions from it with the help of local
 * Actions and Finders, repeatedly refines the odin mentions with OdinRefiners, converts them into EidosMentions,
 * refines those with EidosRefiners, and finally incorporates them into an AnnotatedDocument.
 *
 * OdinRefiners is a collection of functions that each refines (convert, transform, modifies) a Seq[Mention]
 * (odin Mentions) to another Seq[Mention] which will be fed into the next refiner.
 *
 * EidosRefiners do the same, but with EidosMentions: Seq[EidosMention] => Seq[EidosMention].
 *
 * The collections of refiners form a pipeline which can be configured at runtime or even be supplied to
 * Eidos from elsewhere.
 */
class EidosSystem(val components: EidosComponents) {
  // The "copy constructor" below will take cheap-to-update values from the config, but expensive
  // values from eidosSystem.components, if present  It is the new reload().
  def this(config: Config, eidosSystemOpt: Option[EidosSystem] = None) =
      this(new EidosComponentsBuilder(EidosSystem.PREFIX).add(config, eidosSystemOpt.map(_.components)).build())
  def this() = this(EidosSystem.defaultConfig)
  // Python now uses the default, empty constructor above, but the line below remains for documentation purposes.
  // def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val emptyState = new State() // Keep this around for reuse.
  protected val debug = true

  // This abbreviated collection is used in a couple of apps that do not need the entire pipeline.
  // Note: In main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
  // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports.
  // val cagRelevant = keepCAGRelevant(events)
  protected val headOdinRefiners = Seq(
    (odinMentions: Seq[Mention]) => { components.actions.keepMostCompleteEvents(odinMentions) },
    (odinMentions: Seq[Mention]) => { odinMentions.distinct }
  )

  // This is the pipeline for odin Mentions.
  protected def mkOdinRefiners(options: EidosSystem.Options): Seq[EidosSystem.OdinRefiner] = {
    val tailOdinRefiners = Seq(
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

    headOdinRefiners ++ tailOdinRefiners
  }

  // This is the pipeline for EidosMentions.
  protected def mkEidosRefiners(options: EidosSystem.Options): Seq[EidosSystem.EidosRefiner] = Seq(
    (eidosMentions: Seq[EidosMention]) => { components.ontologyHandler.ground(eidosMentions) }
  )

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
    val tokenizedDoc = components.proc.mkDocument(text, keepText = true) // This must now be true.
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
    val initialState = components.entityFinders.foldLeft(emptyState) { (state, entityFinder) =>
      val mentions = entityFinder.find(doc, state)

      state.updated(mentions)
    }

    // It is believed that toVector is required to avoid some race conditions within the engine.
    // The engine's extractFrom returns a Seq which may involve delayed evaluation.
    components.engine.extractFrom(doc, initialState).toVector
  }

  def extractMentionsFrom(doc: Document): Seq[Mention] = {
    val odinMentions = mkMentions(doc)

    refineOdinMentions(headOdinRefiners, odinMentions)
  }

  protected def refine[T](refiners: Seq[EidosSystem.Refiner[T]], mentions: Seq[T]): Seq[T] = {
    val lastMentions = refiners.foldLeft(mentions) { (prevMentions, refiner) =>
      val nextMentions = refiner(prevMentions)

      nextMentions // inspect here
    }

    lastMentions
  }

  def refineOdinMentions(odinRefiners: Seq[EidosSystem.OdinRefiner], odinMentions: Seq[Mention]): Seq[Mention] =
      refine[Mention](odinRefiners, odinMentions)

  def refineEidosMentions(eidosRefiners: Seq[EidosSystem.EidosRefiner], eidosMentions: Seq[EidosMention]): Seq[EidosMention] =
      refine[EidosMention](eidosRefiners, eidosMentions)

  // This could be used with more dynamically configured refiners, especially if made public.
  protected def extractFromDoc(doc: Document, odinRefiners: Seq[EidosSystem.OdinRefiner],
      eidosRefiners: Seq[EidosSystem.EidosRefiner]): AnnotatedDocument = {
    val odinMentions = mkMentions(doc)
    val refinedOdinMentions = refineOdinMentions(odinRefiners, odinMentions)
    val eidosMentions = EidosMention.asEidosMentions(refinedOdinMentions)
    val refinedEidosMentions = refineEidosMentions(eidosRefiners, eidosMentions)
    val annotatedDocument = AnnotatedDocument(doc, refinedEidosMentions)

    annotatedDocument
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(doc: Document, options: EidosSystem.Options, metadata: Metadata): AnnotatedDocument = {
    val odinRefiners = mkOdinRefiners(options)
    val eidosRefiners = mkEidosRefiners(options)

    metadata.attachToDoc(doc)
    extractFromDoc(doc, odinRefiners, eidosRefiners)
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
  type Refiner[T] = Seq[T] => Seq[T]
  type OdinRefiner = Refiner[Mention]
  type EidosRefiner = Refiner[EidosMention]

  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  protected val PREFIX = "EidosSystem"

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
  val EXPAND: Set[String] = CAG_EDGES ++ Set(MIGRATION_LABEL) // StopwordManager

  def defaultConfig: Config = ConfigFactory.load("eidos")

  // Turn off warnings from this class.
  edu.stanford.nlp.ie.NumberNormalizer.setVerbose(false)

  class Options(val cagRelevantOnly: Boolean) {
  }

  object Options {

    def apply(cagRelevantOnly: Boolean): Options = new Options(cagRelevantOnly)
  }
}
