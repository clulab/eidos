package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.context.{GeoNormFinder, TimeNormFinder}

trait Finder {
  // This last argument is optional because not all finders need it and that way a simple default value can be provided.
  def find(doc: Document, initialState: State = new State()): Seq[Mention]
}

object Finder {

  def fromConfig(key: String, config: Config): Seq[Finder] = {
    val finderTypes: List[String] = config[List[String]](key)
    finderTypes.map { finder =>
      finder match {
        case "rulebased" => RuleBasedEntityFinder.fromConfig(config)
        case "gazetteer" => GazetteerEntityFinder.fromConfig(config)
        case "geonorm" => GeoNormFinder.fromConfig(config[Config]("geonorm"))
        case "timenorm" => TimeNormFinder.fromConfig(config[Config]("timenorm"))
        case _ => ???
      }
    }
  }
}