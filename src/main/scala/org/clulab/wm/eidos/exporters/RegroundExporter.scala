package org.clulab.wm.eidos.exporters

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

class RegroundExporter(filename: String, reader: EidosSystem) extends JSONLDExporter(filename, reader) {
  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    FileUtils.printWriterFromFile(filename).autoClose { pw =>
      val regroundedAnnotatedDocs = for {
        ad <- annotatedDocuments
        doc = ad.document
        origMentions = ad.eidosMentions
        regrounded = reader.components.ontologyHandler.ground(origMentions)
      } yield AnnotatedDocument(doc, regrounded)

      val corpus = new JLDCorpus(regroundedAnnotatedDocs)
      val mentionsJSONLD = corpus.serialize()

      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}
