package org.clulab.wm.eidos.refiners

import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosComponents
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment
import org.clulab.wm.eidos.utils.Timer

// If the relevant component is not configured, None should be returned.
// The system then converts the None back into the original Seq[Mention].
// This just simplifies much of the otherwise boilerplate code.
class ProcessorRefiner(val name: String, val refine: Document => Option[Document])

object ProcessorRefiner {

  def mkProcessorRefiners(components: EidosComponents, options: EidosSystem.Options, metadata: Metadata): Seq[ProcessorRefiner] = {
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

  def refineProcessorDocument(processorRefiners: Seq[ProcessorRefiner], doc: Document, useTimer: Boolean): Document = {
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
}
