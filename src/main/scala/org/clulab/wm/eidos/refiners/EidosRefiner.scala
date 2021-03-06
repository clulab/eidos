package org.clulab.wm.eidos.refiners

import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment

import scala.collection.mutable

class EidosRefiner(name: String, val refine: AnnotatedDocument => Option[AnnotatedDocument]) extends Refiner(name)

object EidosRefiner {

  // This is the pipeline for EidosMentions.
  def mkRefiners(components: EidosComponents, options: RefinerOptions): Seq[EidosRefiner] = Seq(
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
        val relevances = RelevanceDocumentAttachment.getRelevance(annotatedDocument.document).getOrElse(Seq.empty)

        relevances.zipWithIndex.foreach { case (relevance, index) =>
          cache(index) = Some(relevance)
        }
        annotatedDocument.allEidosMentions.foreach { eidosMention =>
          eidosMention.classificationOpt = cache.getOrElseUpdate(eidosMention.odinMention.sentence,
            eidosSentenceClassifier.classify(eidosMention.odinMention.sentenceObj))
        }
        annotatedDocument
      }
    })
  )

  def refine(eidosRefiners: Seq[EidosRefiner], annotatedDocument: AnnotatedDocument, useTimer: Boolean): AnnotatedDocument = {
    val lastAnnotatedDocument = eidosRefiners.foldLeft(annotatedDocument) { (prevAnnotatedDocument, refiner) =>
      refiner.time(useTimer) {
        val nextAnnotatedDocument = refiner
            .refine(prevAnnotatedDocument)
            .getOrElse(prevAnnotatedDocument)

        nextAnnotatedDocument // inspect here
      }
    }
    lastAnnotatedDocument
  }
}
