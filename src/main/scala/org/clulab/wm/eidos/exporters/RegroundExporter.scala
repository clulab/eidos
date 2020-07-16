package org.clulab.wm.eidos.exporters

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

class RegroundExporter(filename: String, reader: EidosSystem) extends JSONLDExporter(filename, reader) {
  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    val ontologyHandler = reader.components.ontologyHandler

    FileUtils.printWriterFromFile(filename).autoClose { pw =>
      annotatedDocuments.foreach { annotatedDocument =>
        annotatedDocument.allEidosMentions.foreach { eidosMention =>
          ontologyHandler.ground(eidosMention)
        }

        val corpus = new JLDCorpus(annotatedDocument)
        val mentionsJSONLD = corpus.serialize()

        pw.println(stringify(mentionsJSONLD, pretty = true))
      }
    }
  }
}
