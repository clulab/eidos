package org.clulab.wm.eidos.utils.meta

import org.clulab.wm.eidos.document.Metadata

trait EidosText {
  def getText: String
  def getMetadata: Metadata
}
