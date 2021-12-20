package org.clulab.wm.eidos

import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.components.ComponentsBuilder
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.SeasonFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.serialization.simple.SimpleSerializer

/**
  * This code shows how a custom Eidos reader can be created by configuring the
  * standard one and then adjusting the components that it uses.  This is an
  * alternative to modifying its behavior by inheritance and method overrides.
  * It probably works best when it is entire components that switched on and off.
  * Some of the components are turned on and off at build time via the conf file.
  * The fate of others is controlled at object creation time by manipulation of
  * components.
  */
object SimpleEidos {
  lazy val components: EidosComponents = {
    // Check the conf file which turns off grounding and turns on time and geo Finders.
    val config = ConfigFactory.load("simpleEidos")
    // Create the specified components for use with an EidosSystem instance.
    val components = new ComponentsBuilder(config, EidosSystem.PREFIX).build()

    // Make sure the settings are not too different from what we're expecting.
    require(components.useGeoNorm)
    require(components.useTimeNorm)
    require(components.ontologyHandlerOpt.isEmpty || components.ontologyHandlerOpt.get.ontologyGrounders.isEmpty)
    components
  }
  lazy val finders: Seq[Finder] = components.findersOpt.get

  def apply(useGeoNorm: Boolean, useTimeNorm: Boolean): EidosSystem = {
    // Only keep Geo and Time if they are specified by the incoming arguments.
    // In this way they can be determined at run time.
    val simpleFinders = finders.filter { finder =>
      finder match {
        case _: GeoNormFinder => useGeoNorm
        case _: TimeNormFinder => useTimeNorm
        case _: SeasonFinder => useGeoNorm && useTimeNorm
        case _ => true
      }
    }
    // Make a copy of all the components, but substitute in the configured finders.
    val simpleComponents = components.copy(findersOpt = Some(simpleFinders))

    // Create an EidosSystem with the desired components.
    new EidosSystem(simpleComponents)
  }

  def main(args: Array[String]): Unit = {

    def extract(eidosSystem: EidosSystem, text: String): Unit = {
      val annotatedDocument = eidosSystem.extractFromText(text)
      val simpleSerializer = SimpleSerializer(annotatedDocument)

      simpleSerializer.serialize()
    }

    val text = "This is a test.  Water trucking in Ethiopia has decreased over August due to the cost of fuel."

    // Make four different versions of EidosSystem with all the possible combinations of
    // useGeoNorm and useTimeNorm.  The components are only ever loaded once.  They are
    // just used in different combinations by each EidosSystem.
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
