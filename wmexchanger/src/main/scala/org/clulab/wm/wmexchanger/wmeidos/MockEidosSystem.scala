package org.clulab.wm.wmexchanger.wmeidos

import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.Metadata

class MockEidosSystem extends EidosSystemish {

  def getEmptyAnnotatedDocument(idOpt: Option[String]): AnnotatedDocument = {
    val document = new Document(Array.empty)
    document.id = idOpt

    val annotatedDocument = AnnotatedDocument(document, Seq.empty)

    annotatedDocument
  }

  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument = {
    Thread.sleep(5000)
    getEmptyAnnotatedDocument(metadata.idOpt)
  }
}
