package org.clulab.wm.eidos.apps.groundings

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils

object GroundFromText extends App {
  val cagRelevantOnly = false
  val text = "Water trucking has decreased due to the cost of fuel."
  val reader = new EidosSystem()
  val annotatedDocument = reader.extractFromText(text, cagRelevantOnly)

  DisplayUtils.displayEidosMentions(annotatedDocument.eidosMentions, annotatedDocument.document, printDeps = true)
}
