package org.clulab.wm.eidos.refiners

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.utils.Timer

class ProcessorRefiner(name: String, val refine: String => Option[Document]) extends Refiner(name)

object ProcessorRefiner {

  // This is really a Converter and the conversion can only happen once.
  def mkRefiner(components: EidosComponents, options: RefinerOptions): ProcessorRefiner = {
    new ProcessorRefiner("Processors.mkDocument", (text: String) => {
      components.procOpt.map { proc =>
        proc.mkDocument(text, keepText = true) // This must now be true.
      }
    })
  }

  def refine(textRefiner: ProcessorRefiner, text: String, useTimer: Boolean): Document = {
    val document = {
      Timer.time("Run " + textRefiner.name, useTimer) {
        val document = textRefiner
            .refine(text)
            .getOrElse(Document(Array.empty[Sentence]))

        document
      }
    }
    document
  }
}
