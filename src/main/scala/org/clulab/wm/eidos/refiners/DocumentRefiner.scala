package org.clulab.wm.eidos.refiners

import org.clulab.processors.Document
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment

// If the relevant component is not configured, None should be returned.
// The system then converts the None back into the original Seq[Mention].
// This just simplifies much of the otherwise boilerplate code.
class DocumentRefiner(name: String, val refine: Document => Option[Document]) extends Refiner(name)

object DocumentRefiner {

  def mkAnnotateRefiners(components: EidosComponents, options: RefinerOptions, metadata: Metadata): Seq[DocumentRefiner] = {
    // The metadata, in particular the DCT must be present before annotation.
    Seq(
      new DocumentRefiner("MetadataHandler", (doc: Document) => {
        Some {
          metadata.attachToDoc(doc)
          doc
        }
      }),
      new DocumentRefiner("Processors.annotate", (document: Document) => {
        components.procOpt.map { proc =>
          proc.annotate(document)
        }
      })
    )
  }

  def mkRefiners(components: EidosComponents, options: RefinerOptions, metadata: Metadata): Seq[DocumentRefiner] = {
    Seq(
      new DocumentRefiner("SentenceClassifier-sentences", (doc: Document) => {
        components.eidosSentenceClassifierOpt.map { eidosSentenceClassifier =>
          val relevanceOpts = doc.sentences.map { sent => eidosSentenceClassifier.classify(sent) }

          RelevanceDocumentAttachment.setRelevanceOpt(doc, relevanceOpts)
          doc
        }
      })
    )
  }

  def refine(documentRefiners: Seq[DocumentRefiner], doc: Document, useTimer: Boolean): Document = {
    val lastDoc = documentRefiners.foldLeft(doc) { (prevDoc, refiner) =>
      refiner.time(useTimer) {
        val nextDoc = refiner
            .refine(prevDoc)
            .getOrElse(prevDoc)

        nextDoc
      }
    }
    lastDoc
  }
}
