package org.clulab.wm.eidos

import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.components.ComponentsBuilder
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.SeasonFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.utils.DisplayUtils

object SimpleEidos {
  lazy val components = {
    val config = ConfigFactory.load("simpleEidos")
    val components = new ComponentsBuilder(config, EidosSystem.PREFIX).build()

    require(components.useGeoNorm)
    require(components.useTimeNorm)
    require(components.ontologyHandlerOpt.isEmpty || components.ontologyHandlerOpt.get.ontologyGrounders.isEmpty)
    components
  }
  lazy val finders = components.findersOpt.get

  def apply(useGeoNorm: Boolean, useTimeNorm: Boolean): EidosSystem = {
    val simpleFinders = finders.filter { finder =>
      finder match {
        case _: GeoNormFinder => useGeoNorm
        case _: TimeNormFinder => useTimeNorm
        case _: SeasonFinder => useGeoNorm && useTimeNorm
        case _ => true
      }
    }
    val simpleComponents = components.copy(findersOpt = Some(simpleFinders))

    new EidosSystem(simpleComponents)
  }

  def main(args: Array[String]): Unit = {

    def extract(eidosSystem: EidosSystem, text: String): Unit = {
      val annotatedDocument = eidosSystem.extractFromText(text)
      annotatedDocument.odinMentions.foreach(DisplayUtils.displayMention)
    }

    val text = "Water trucking in Ethopida has decreased over August due to the cost of fuel."

    val eidosSystemNone = SimpleEidos(false, false)
    extract(eidosSystemNone, text)

    val eidosSystemGeo = SimpleEidos(true, false)
    extract(eidosSystemGeo, text)

    val eidosSystemTime = SimpleEidos(false, true)
    extract(eidosSystemTime, text)

    val eidosSystemBoth = SimpleEidos(true, true)
    extract(eidosSystemBoth, text)
  }
}
