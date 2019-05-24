package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State}
import org.clulab.processors.Document

trait Finder {
  def extract(doc: Document, initialState: State = new State()): Seq[Mention]
}

object Finder {

  def fromConfig(key: String, config: Config): Seq[Finder] = {
    val finderTypes: List[String] = config[List[String]](key)
    finderTypes.map { finder =>
      finder match {
        case "rulebased" => RuleBasedEntityFinder.fromConfig(config)
        case "gazetteer" => GazetteerEntityFinder.fromConfig(config)
        case _ => ???
      }
    }
  }
}