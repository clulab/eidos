package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{ExtractorEngine, Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.TagSet

class OdinFinder(val expanderOpt: Option[Expander], val engine: ExtractorEngine) extends Finder {

  def find(doc: Document, initialState: State = new State()): Seq[Mention] = {
    val baseExtractions = engine.extractFrom(doc, initialState)

    expanderOpt
        .map(_.expand(baseExtractions))
        .getOrElse(baseExtractions)
  }
}

object OdinFinder {
  def fromConfig(config: Config, tagSet: TagSet): OdinFinder = {
    val rulesPath = config[String]("rulesPath")
    val rules = FileUtils.getTextFromResource(rulesPath)
    val actions = EidosActions.fromConfig(config[Config]("actions"), tagSet)
    val useGlobalAction = config[Boolean]("useGlobalAction")
    val entityEngine = if (useGlobalAction) {
      ExtractorEngine(rules, actions, actions.globalAction)
    } else {
      ExtractorEngine(rules, actions)
    }

    val expanderConfigOpt = config.get[Config]("expander")
    val expanderOpt = expanderConfigOpt.map(Expander.fromConfig(_, tagSet))

    new OdinFinder(expanderOpt, entityEngine)
  }
}
