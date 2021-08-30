package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils

import java.io.PrintWriter

class GroundExporter(filename: String, reader: EidosSystem) extends JSONLDExporter(filename, reader) {
  val ontologyHandler: OntologyHandler = reader.components.ontologyHandlerOpt.get

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    annotatedDocument.allEidosMentions.foreach { eidosMention =>
      ontologyHandler.ground(eidosMention)
    }
    super.export(annotatedDocument)
    if (GroundExporter.debug)
      debug()

    def debug(): Unit = {

      def debugGrounding(message: String, groundingOpt: Option[OntologyGrounding], printWriter: PrintWriter): Unit = {
        printWriter.println(message)
        groundingOpt match {
          case None => printWriter.println("no grounding...")
          case Some(grounding) =>
            grounding.grounding.foreach { gr =>
              printWriter.println(s"  --> grounding: ${gr.name}")
              printWriter.println(s"      score: ${gr.score}")
            }
        }
      }

      FileUtils.printWriterFromFile(filename + ".debug.txt").autoClose { printWriter =>
        annotatedDocument.eidosMentions.filter(_.odinMention matches "Causal").foreach { em =>
          val args = em.eidosArguments("cause") ++ em.eidosArguments("effect")

          args.foreach { eidosMention: EidosMention =>
            val text = eidosMention.odinMention.text
            val groundingComp = eidosMention.grounding.get("wm_compositional")
            val groundingFlat = eidosMention.grounding.get("wm_flattened")

            printWriter.println(s"text: $text")
            debugGrounding("Compositional Grounding:", groundingComp, printWriter)
            debugGrounding("Flat Grounding:", groundingFlat, printWriter)
            printWriter.println()
          }
        }
      }
    }
  }
}

object GroundExporter {
  val debug = false
}
