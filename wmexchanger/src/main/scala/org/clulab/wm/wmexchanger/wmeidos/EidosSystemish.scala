package org.clulab.wm.wmexchanger.wmeidos

import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.Metadata

trait EidosSystemish {
  def getEmptyAnnotatedDocument(idOpt: Option[String]): AnnotatedDocument
  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument
}
