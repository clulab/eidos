package org.clulab.wm.eidos.apps.groundings

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils

object GroundFromText extends App {

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  DisplayUtils.displayEidosMentions(annotatedDocument.eidosMentions, annotatedDocument.document, printDeps = true)
}
