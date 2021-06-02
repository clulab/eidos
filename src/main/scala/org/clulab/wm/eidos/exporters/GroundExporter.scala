package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument

class GroundExporter(filename: String, reader: EidosSystem) extends JSONLDExporter(filename, reader) {
  val ontologyHandler = reader.components.ontologyHandlerOpt.get

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    annotatedDocument.allEidosMentions.foreach { eidosMention =>
      ontologyHandler.ground(eidosMention)
    }
    super.export(annotatedDocument)
  }
}
