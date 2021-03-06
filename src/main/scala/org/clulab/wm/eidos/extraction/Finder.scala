package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.context.{GeoNormFinder, SeasonFinder, TimeNormFinder}
import org.clulab.wm.eidoscommon.StopwordManaging
import org.clulab.wm.eidoscommon.TagSet

trait Finder {
  def find(doc: Document, initialState: State = new State()): Seq[Mention]
}

object Finder {

  def fromConfig(key: String, config: Config, tagSet: TagSet, stopwordManaging: StopwordManaging): Seq[Finder] = {
    val finderTypes: List[String] = config[List[String]](key)
    finderTypes.map { finder =>
      finder match {
        case "rulebased" => RuleBasedEntityFinder.fromConfig(config, tagSet, stopwordManaging)
        case "gazetteer" => GazetteerEntityFinder.fromConfig(config, tagSet)
        case "geonorm" => GeoNormFinder.fromConfig(config[Config]("geonorm"))
        case "timenorm" => TimeNormFinder.fromConfig(config[Config]("timenorm"))
        case "seasons" => SeasonFinder.fromConfig(config[Config]("seasons"))
        case "context" => OdinFinder.fromConfig(config[Config]("context"), tagSet)
        case "causal" => OdinFinder.fromConfig(config[Config]("causal"), tagSet)
        case _ => ???
      }
    }
  }
}