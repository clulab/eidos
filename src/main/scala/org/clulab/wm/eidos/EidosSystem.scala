package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment
import org.clulab.wm.eidos.utils.Timer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

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
 * In the meantime there are also ProcessorRefiners which work on the Documents.
 *
 * The collections of refiners form a pipeline which can be configured at runtime or even be supplied to
 * Eidos from elsewhere.
 */
class EidosSystem(val components: EidosComponents) {
  // The "copy constructor" below will take cheap-to-update values from the config, but expensive
  // values from eidosSystem.components, if present  It is the new reload().
  def this(config: Config, eidosSystemOpt: Option[EidosSystem] = None) =
      this(new EidosComponentsBuilder(config, EidosSystem.PREFIX, eidosSystemOpt.map(_.components)).build())
  def this() = this(EidosSystem.defaultConfig)
  // Python now uses the default, empty constructor above, but the line below remains for documentation purposes.
  // def this(x: Object) = this() // Dummy constructor crucial for Python integration

  protected val emptyState = new State() // Keep this around for reuse.
  protected val debug = true
  protected val useTimer = false

  protected def mkProcessorRefiners(options: EidosSystem.Options, metadata: Metadata): Seq[ProcessorRefiner] = {
    Seq(
      new ProcessorRefiner("SentenceClassifier-sentences", (doc: Document) => {
        components.eidosSentenceClassifierOpt.map { eidosSentenceClassifier =>
          val relevanceOpts = doc.sentences.map { sent => eidosSentenceClassifier.classify(sent) }

          RelevanceDocumentAttachment.setRelevanceOpt(doc, relevanceOpts)
          doc
        }
      }),
      new ProcessorRefiner("MetadataHandler", (doc: Document) => {
        Some {
          metadata.attachToDoc(doc)
          doc
        }
      })
    )
  }

  // This abbreviated collection is used in a couple of apps that do not need the entire pipeline.
  // Note: In main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
  // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports.
  // val cagRelevant = keepCAGRelevant(events)
  protected val headOdinRefiners: Seq[OdinRefiner] = Seq(
    // Merge attachments: look for mentions with the same span and label and merge their attachments so none get lost
    new OdinRefiner("AttachmentHandler",        (odinMentions: Seq[Mention]) => {
      components.attachmentHandlerOpt.map(_.mergeAttachments(odinMentions))
    }),
    // Keep the most complete version of redundant mentions, should be done *after* attachments have been merged
    new OdinRefiner("MostCompleteEventsKeeper", (odinMentions: Seq[Mention]) => {
      components.mostCompleteEventsKeeperOpt.map(_.keepMostCompleteEvents(odinMentions))
    }),
    // Make sure there are no duplicated Mentions
    new OdinRefiner("Distinct",                 (odinMentions: Seq[Mention]) => {
      Some(odinMentions.distinct)
    })
  )


  // This is the pipeline for odin Mentions.
  protected def mkOdinRefiners(options: EidosSystem.Options): Seq[OdinRefiner] = {
    val tailOdinRefiners = Seq(
      // Try to find additional causal relations by resolving simple event coreference
      new OdinRefiner("CorefHandler",           (odinMentions: Seq[Mention]) => {
        components.corefHandlerOpt.map(_.resolveCoref(odinMentions))
      }),
      // Expand concepts that aren't part of causal events, but which we are keeping and outputting
      new OdinRefiner("ConceptExpander",        (odinMentions: Seq[Mention]) => {
        components.conceptExpanderOpt.map(_.expand(odinMentions))
      }),
      // Expand any nested arguments that got missed before
      new OdinRefiner("NestedArgumentExpander", (odinMentions: Seq[Mention]) => {
        components.nestedArgumentExpanderOpt.map(_.traverse(odinMentions))
      }),
      // Filtering based on contentfulness
      new OdinRefiner("StopwordManager",        (odinMentions: Seq[Mention]) => {
        // This exception is dependent on runtime options.
        if (options.cagRelevantOnly) components.stopwordManagerOpt.map(_.keepCAGRelevant(odinMentions))
        else Some(odinMentions)
      }),
      // Annotate hedging
      new OdinRefiner("HedgingHandler",         (odinMentions: Seq[Mention]) => {
        components.hedgingHandlerOpt.map(_.detectHypotheses(odinMentions))
      }),
      // Annotate negation
      new OdinRefiner("NegationHandler",        (odinMentions: Seq[Mention]) => {
        components.negationHandlerOpt.map(_.detectNegations(odinMentions))
      })
    )

    headOdinRefiners ++ tailOdinRefiners
  }

  // This is the pipeline for EidosMentions.
  protected def mkEidosRefiners(options: EidosSystem.Options): Seq[EidosRefiner] = Seq(
    new EidosRefiner("OntologyHandler",   (annotatedDocument: AnnotatedDocument) => {
      components.ontologyHandlerOpt.map { ontologyHandler =>
        annotatedDocument.allEidosMentions.foreach(ontologyHandler.ground)
        annotatedDocument
      }
    }),
    new EidosRefiner("AdjectiveGrounder", (annotatedDocument: AnnotatedDocument) => {
      components.adjectiveGrounderOpt.map { adjectiveGrounder =>
        annotatedDocument.allEidosMentions.foreach(_.groundAdjectives(adjectiveGrounder))
        annotatedDocument
      }
    }),
    new EidosRefiner("SentenceClassifier-mentions", (annotatedDocument: AnnotatedDocument) => {
      components.eidosSentenceClassifierOpt.map { eidosSentenceClassifier =>
        // This maps sentence index to the sentence classification so that sentences aren't classified twice.
        val cache: mutable.HashMap[Int, Option[Float]] = mutable.HashMap.empty
        // Retrieve these back from the document?

        annotatedDocument.allEidosMentions.foreach { eidosMention =>
          eidosMention.classificationOpt = cache.getOrElseUpdate(eidosMention.odinMention.sentence,
              eidosSentenceClassifier.classify(eidosMention.odinMention.sentenceObj))
        }
        annotatedDocument
      }
    })
  )

  // ---------------------------------------------------------------------------------------------
  //                                 Annotation Methods
  // ---------------------------------------------------------------------------------------------

  def annotateDoc(doc: Document): Document = {
    // It is assumed and not verified that the document has _not_ already been annotated.
    components.procOpt.map { proc =>
      Timer.time("Run Processors.annotate", useTimer) {
        proc.annotate(doc)
        doc
      }
    }
    .getOrElse(doc)
  }

  // Annotate the text using a Processor and then populate lexicon labels.
  // If there is a document time involved, please place it in the metadata
  // and use one of the calls that takes it into account.
  def annotate(text: String): Document = {
    val annotatedDoc = components.procOpt.map { proc =>
      val tokenizedDoc = Timer.time("Run Processors.mkDocument", useTimer) {
        proc.mkDocument(text, keepText = true) // This must now be true.
      }
      annotateDoc(tokenizedDoc)
    }
    .getOrElse(Document(Array.empty[Sentence]))

    annotatedDoc
  }

  // ---------------------------------------------------------------------------------------------
  //                                 Extraction Methods
  // ---------------------------------------------------------------------------------------------

  protected def mkMentions(doc: Document): Seq[Mention] = {
    require(doc.text.isDefined)

    components.findersOpt.map { finders =>
      // Perform the information extraction in the sequence set in the config, using Finders
      val extractions = finders.foldLeft(emptyState) { (state, finder) =>
        Timer.time("Run " + finder.getClass.getSimpleName, useTimer) {
          val mentions = finder.find(doc, state).toVector

          state.updated(mentions)
        }
      }

      // It is believed that toVector is required to avoid some race conditions within the engine.
      // The engine's extractFrom returns a Seq which may involve delayed evaluation.
      extractions.allMentions
    }
    .getOrElse(Seq.empty[Mention])
  }

  def refineProcessorDocument(processorRefiners: Seq[ProcessorRefiner], doc: Document): Document = {
    val lastDoc = processorRefiners.foldLeft(doc) { (prevDoc, refiner) =>
      Timer.time("Run " + refiner.name, useTimer) {
        val nextDoc = refiner
            .refine(prevDoc)
            .getOrElse(prevDoc)

        nextDoc
      }
    }
    lastDoc
  }

  // This is a partial version, mostly legacy.
  def extractMentionsFrom(doc: Document): Seq[Mention] = {
    val odinMentions = mkMentions(doc)

    refineOdinMentions(headOdinRefiners, odinMentions)
  }

  def refineOdinMentions(odinRefiners: Seq[OdinRefiner], odinMentions: Seq[Mention]): Seq[Mention] = {
    val lastMentions = odinRefiners.foldLeft(odinMentions) { (prevMentions, refiner) =>
      Timer.time("Run " + refiner.name, useTimer) {
        val nextMentions = refiner
            .refine(prevMentions)
            .getOrElse(prevMentions)

        nextMentions // inspect here
      }
    }
    lastMentions
  }

  def refineEidosMentions(eidosRefiners: Seq[EidosRefiner], annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    val lastAnnotatedDocument = eidosRefiners.foldLeft(annotatedDocument) { (prevAnnotatedDocument, refiner) =>
      Timer.time("Run " + refiner.name, useTimer) {
        val nextAnnotatedDocument = refiner
            .refine(prevAnnotatedDocument)
            .getOrElse(prevAnnotatedDocument)

        nextAnnotatedDocument // inspect here
      }
    }
    lastAnnotatedDocument
  }

  // This could be used with more dynamically configured refiners, especially if made public.
  // Refining is where, e.g., grounding and filtering happens.
  protected def extractFromDoc(doc: Document, processorRefiners: Seq[ProcessorRefiner], odinRefiners: Seq[OdinRefiner],
      eidosRefiners: Seq[EidosRefiner]): AnnotatedDocument = {
    val odinMentions = mkMentions(doc)
    val refinedOdinMentions = refineOdinMentions(odinRefiners, odinMentions)
    val annotatedDocument = AnnotatedDocument(doc, refinedOdinMentions)
    val refinedAnnotatedDocument = refineEidosMentions(eidosRefiners, annotatedDocument)

    refinedAnnotatedDocument
  }

  // MAIN PIPELINE METHOD if given doc
  def extractFromDoc(annotatedDoc: Document, options: EidosSystem.Options, metadata: Metadata): AnnotatedDocument = {
    val processorRefiners = mkProcessorRefiners(options, metadata)
    val odinRefiners = mkOdinRefiners(options)
    val eidosRefiners = mkEidosRefiners(options)

    extractFromDoc(annotatedDoc, processorRefiners, odinRefiners, eidosRefiners)
  }

  // Legacy version
  def extractFromDoc(
    annotatedDoc: Document,
    cagRelevantOnly: Boolean = true,
    dctOpt: Option[DCT] = None,
    idOpt: Option[String] = None
  ): AnnotatedDocument = {
    extractFromDoc(annotatedDoc, EidosSystem.Options(cagRelevantOnly), Metadata(dctOpt, idOpt))
  }

  // MAIN PIPELINE METHOD if given text
  def extractFromText(text: String, options: EidosSystem.Options, metadata: Metadata): AnnotatedDocument = {
    val annotatedDoc = annotate(text)

    extractFromDoc(annotatedDoc, options, metadata)
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

// If the relevant component is not configured, None should be returned.
// The system then converts the None back into the original Seq[Mention].
// This just simplifies much of the otherwise boilerplate code.

class ProcessorRefiner(val name: String, val refine: Document => Option[Document])

class OdinRefiner(val name: String, val refine: Seq[Mention] => Option[Seq[Mention]])

class EidosRefiner(val name: String, val refine: AnnotatedDocument => Option[AnnotatedDocument])

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

  class Options(val cagRelevantOnly: Boolean) {
  }

  object Options {

    def apply(cagRelevantOnly: Boolean = true): Options = new Options(cagRelevantOnly)
  }
}
