package org.clulab.wm.eidos.apps.groundings

import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosApp
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.exporters.GroundingInsightExporter
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DisplayUtils
import org.clulab.wm.eidos.utils.GroundingInfoSupplier

object GroundFromText extends EidosApp {

  class EidosGroundingInsight(eidosSystem: EidosSystem, config: Config) extends GroundingInfoSupplier {
    protected val groundingInsightExporter = new GroundingInsightExporter("", eidosSystem, config)

    def supplyGroundingInfo(m: EidosMention): String = groundingInsightExporter.mentionGroundingInfo(m)
  }

  val groundingInsights = false
  val cagRelevantOnly = false
  val text = "Water trucking has decreased due to the cost of fuel."
  val reader = new EidosSystem()
  val annotatedDocument = reader.extractFromText(text, cagRelevantOnly)
  val eidosGroundingInsightOpt =
      if (groundingInsights) Some(new EidosGroundingInsight(reader, config))
      else None

  DisplayUtils.displayEidosMentions(annotatedDocument.eidosMentions, annotatedDocument.document,
      printDeps = true, eidosGroundingInsightOpt)
}
