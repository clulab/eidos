package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.utils.Closer._
import org.clulab.wm.eidos.utils.FileUtils

class DebugGroundingExporter(filename: String) extends Exporter {

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    FileUtils.printWriterFromFile(filename).autoClose { pw =>
      annotatedDocument.eidosMentions.filter(_.odinMention matches "Causal").foreach { em =>
        val args = em.eidosArguments("cause") ++ em.eidosArguments("effect")
        val info = args.map(m => (m.odinMention.text, m.grounding.get("wm_compositional").flatMap(_.headOption)))
        for ((text, groundingClass) <- info) {
          pw.println(s"text: ${text}")
          groundingClass match {
            case None => pw.println("no grounding...")
            case Some(grounding) =>
              pw.println(s"grounding: ${grounding.name}")
              pw.println(s"score: ${grounding.score}")
          }
          pw.println()
        }

      }

    }
  }

}
