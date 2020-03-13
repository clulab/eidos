package org.clulab.wm.eidos.exporters

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils


case class JSONLDExporter(filename: String, reader: EidosSystem) extends Exporter {

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    FileUtils.printWriterFromFile(filename).autoClose { pw =>
      val corpus = new JLDCorpus(annotatedDocument)
      val mentionsJSONLD = corpus.serialize()

      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}