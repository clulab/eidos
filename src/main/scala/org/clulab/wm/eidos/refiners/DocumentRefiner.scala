package org.clulab.wm.eidos.refiners

import org.clulab.processors.Document
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment
import org.clulab.wm.eidos.utils.Timer

// If the relevant component is not configured, None should be returned.
// The system then converts the None back into the original Seq[Mention].
// This just simplifies much of the otherwise boilerplate code.
class DocumentRefiner(val name: String, val refine: Document => Option[Document])

object DocumentRefiner {

  def mkAnnotateRefiner(components: EidosComponents, options: RefinerOptions): DocumentRefiner = {
    new DocumentRefiner("Processors.annotate", (document: Document) => {
      components.procOpt.map { proc =>
        proc.annotate(document)
      }
    })
  }

  def mkRefiners(components: EidosComponents, options: RefinerOptions, metadata: Metadata): Seq[DocumentRefiner] = {
    Seq(
      new DocumentRefiner("SentenceClassifier-sentences", (doc: Document) => {
        components.eidosSentenceClassifierOpt.map { eidosSentenceClassifier =>
          val relevanceOpts = doc.sentences.map { sent => eidosSentenceClassifier.classify(sent) }

          RelevanceDocumentAttachment.setRelevanceOpt(doc, relevanceOpts)
          doc
        }
      }),
      new DocumentRefiner("MetadataHandler", (doc: Document) => {
        Some {
          metadata.attachToDoc(doc)
          doc
        }
      })
    )
  }

  def refine(documentRefiners: Seq[DocumentRefiner], doc: Document, useTimer: Boolean): Document = {
    val lastDoc = documentRefiners.foldLeft(doc) { (prevDoc, refiner) =>
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
