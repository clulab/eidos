package org.clulab.wm.eidos.exporters

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils

class RegroundExporter(filename: String, reader: EidosSystem) extends JSONLDExporter(filename, reader) {
  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    val ontologyHandler = reader.components.ontologyHandler
    // Reground
    annotatedDocument.allEidosMentions.foreach { eidosMention =>
      ontologyHandler.ground(eidosMention)
    }
    super.export(annotatedDocument)

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

//  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
//    val ontologyHandler = reader.components.ontologyHandler
//
//    FileUtils.printWriterFromFile(filename).autoClose { pw =>
//      annotatedDocuments.foreach { annotatedDocument =>
//        annotatedDocument.allEidosMentions.foreach { eidosMention =>
//          ontologyHandler.ground(eidosMention)
//        }
//
//        val corpus = new JLDCorpus(annotatedDocument)
//        val mentionsJSONLD = corpus.serialize()
//
//        pw.println(stringify(mentionsJSONLD, pretty = true))
//      }
//    }
//  }
}
