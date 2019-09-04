package org.clulab.wm.eidos.expansion

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State}

import scala.util.matching.Regex

case class Dependencies(validIncoming: Set[Regex], invalidIncoming: Set[Regex], validOutgoing: Set[Regex], invalidOutgoing: Set[Regex])


trait Expander {
  def expand(ms: Seq[Mention], avoidState: State = new State()): Seq[Mention]
}

object Expander {
  def fromConfig(config: Config): Expander = {
    val expandType: String = config[String]("expansionType") // fixme
    expandType match {
      case "textbound" => TextBoundExpander.fromConfig(config) // todo: check about scoping with these nested configs
      case "argument" => ArgumentExpander.fromConfig(config)
      case _ => ???
    }
  }
}