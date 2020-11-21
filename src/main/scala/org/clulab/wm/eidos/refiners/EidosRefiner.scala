package org.clulab.wm.eidos.refiners

import org.clulab.wm.eidos.EidosComponents
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.utils.Timer

import scala.collection.mutable

class EidosRefiner(val name: String, val refine: AnnotatedDocument => Option[AnnotatedDocument])

object EidosRefiner {

  // This is the pipeline for EidosMentions.
  def mkEidosRefiners(components: EidosComponents, options: EidosSystem.Options): Seq[EidosRefiner] = Seq(
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

  def refineEidosMentions(eidosRefiners: Seq[EidosRefiner], annotatedDocument: AnnotatedDocument, useTimer: Boolean): AnnotatedDocument = {
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
}
