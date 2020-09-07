package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.utils.Closer._
import org.clulab.wm.eidos.utils.FileUtils

class DebugGroundingExporter(filename: String) extends Exporter {

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    FileUtils.printWriterFromFile(filename + ".debug.txt").autoClose { pw =>
      annotatedDocument.eidosMentions.filter(_.odinMention matches "Causal").foreach { em =>
        val args = em.eidosArguments("cause") ++ em.eidosArguments("effect")
        val info = args.map(m => (m.odinMention.text, m.grounding.get("wm_compositional"), m.grounding.get("wm_flattened")))
        for ((text, groundingComp, groundingFlat) <- info) {
          pw.println(s"text: ${text}")
          pw.println(s"Compositional Grounding:")
          groundingComp match {
            case None => pw.println("no grounding...")
            case Some(grounding) =>
              grounding.grounding.foreach { gr =>
                pw.println(s"  --> grounding: ${gr.name}")
                pw.println(s"      score: ${gr.score}")
              }
          }
          pw.println(s"Flat Grounding:")
          groundingFlat match {
            case None => pw.println("no grounding...")
            case Some(grounding) =>
              grounding.grounding.foreach { gr =>
                pw.println(s"  --> grounding: ${gr.name}")
                pw.println(s"      score: ${gr.score}")
              }
          }
          pw.println()
        }

      }
    }
  }

}
