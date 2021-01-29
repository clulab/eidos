package org.clulab.wm.eidos

import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.components.ComponentsBuilder
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.SeasonFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.serialization.simple.SimpleSerializer

object SimpleEidos {
  lazy val components: EidosComponents = {
    val config = ConfigFactory.load("simpleEidos")
    val components = new ComponentsBuilder(config, EidosSystem.PREFIX).build()

    require(components.useGeoNorm)
    require(components.useTimeNorm)
    require(components.ontologyHandlerOpt.isEmpty || components.ontologyHandlerOpt.get.ontologyGrounders.isEmpty)
    components
  }
  lazy val finders: Seq[Finder] = components.findersOpt.get

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
      val simpleSerializer = SimpleSerializer(annotatedDocument)

      simpleSerializer.serialize()
    }

    val text = "This is a test.  Water trucking in Ethiopia has decreased over August due to the cost of fuel."

    val eidosSystemNone = SimpleEidos(useGeoNorm = false, useTimeNorm = false)
    extract(eidosSystemNone, text)

    val eidosSystemGeo = SimpleEidos(useGeoNorm = true, useTimeNorm = false)
    extract(eidosSystemGeo, text)

    val eidosSystemTime = SimpleEidos(useGeoNorm = false, useTimeNorm = true)
    extract(eidosSystemTime, text)

    val eidosSystemBoth = SimpleEidos(useGeoNorm = true, useTimeNorm = true)
    extract(eidosSystemBoth, text)
  }
}
