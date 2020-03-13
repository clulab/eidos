package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{ExtractorEngine, Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.utils.FileUtils

class OdinFinder(val expander: Option[Expander], val engine: ExtractorEngine) extends Finder {


  def find(doc: Document, initialState: State = new State()): Seq[Mention] = engine.extractFrom(doc, initialState)

}

object OdinFinder {
  def fromConfig(config: Config): OdinFinder = {
    val rulesPath = config[String]("rulesPath")
    val rules = FileUtils.getTextFromResource(rulesPath)
    val actions = EidosActions.fromConfig(config[Config]("actions"))
    val entityEngine = ExtractorEngine(rules, actions)

    val expanderConfig = config.get[Config]("expander")
    val expander: Option[Expander] = expanderConfig.map(Expander.fromConfig)

    new OdinFinder(expander, entityEngine)
  }
}
