package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigObject
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.Processor
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.utils._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class EidosComponents(protected val config: Config, eidosComponentsOpt: Option[EidosComponents]) {

  def matches(config: Config): Boolean = this.config == config

  def getConfig(path: String): Config = config.getConfig(path)

  def root: ConfigObject = config.root

  protected val reloading: Boolean = eidosComponentsOpt.isDefined

  EidosComponents.logger.info((if (reloading) "Reloading" else "Loading") + " config...")

  protected val eidosConf: Config = config[Config](EidosSystem.PREFIX)

  val (
    language: String,
    proc: Processor,
    negationHandler: NegationHandler,
    documentFilter: DocumentFilter,
    stopwordManager: StopwordManager,
    ontologyHandler: OntologyHandler,
    multiOntologyGrounder: MultiOntologyGrounder
  ) = if (reloading) {
    val config = eidosComponentsOpt.get
    // We don't usually reload into a different language and this is expensive, so reuse these when possible.
    val language = config.language
    val proc = config.proc
    // This only depends on language, not any settings, so a reload is safe.
    val negationHandler = config.negationHandler
    // This is only dependent on proc, which is not reloaded, so don't reload here.
    val documentFilter = config.documentFilter
    val stopwordManager = config.stopwordManager
    // This involves reloading of very large vector files and is what we're trying to avoid.
    val ontologyHandler: OntologyHandler = config.ontologyHandler
    val multiOntologyGrounder = config.multiOntologyGrounder

    (language, proc, negationHandler, documentFilter, stopwordManager, ontologyHandler, multiOntologyGrounder)
  }
  else {
    val language = eidosConf[String]("language")
    val proc = {
      EidosComponents.logger.info("Loading processor...")
      language match {
        case "english" => new FastNLPProcessor
        case "spanish" => new SpanishCluProcessor
        case "portuguese" => new PortugueseCluProcessor
      }
    }
    val negationHandler = NegationHandler(language)
    // Prunes sentences form the Documents to reduce noise/allow reasonable processing time
    val documentFilter = FilterByLength(proc, cutoff = 150)

    val stopwordManager = StopwordManager.fromConfig(config)
    val ontologyHandler: OntologyHandler = OntologyHandler.load(config[Config]("ontologies"), proc, stopwordManager)
    // Ontologies
    val multiOntologyGrounder = ontologyHandler.ontologyGrounders

    (language, proc, negationHandler, documentFilter, stopwordManager, ontologyHandler, multiOntologyGrounder)
  }

  val actions: EidosActions = EidosActions.fromConfig(config[Config]("actions"))
  val engine: ExtractorEngine = { // ODIN component
    val masterRulesPath: String = eidosConf[String]("masterRulesPath")
    val masterRules = FileUtils.getTextFromResource(masterRulesPath)

    ExtractorEngine(masterRules, actions, actions.globalAction)
  }

  // This seems to be unused
  // protected val taxonomyPath: String = eidosConf[String]("taxonomyPath")

  // Hedging
  val hedgingHandler: HypothesisHandler = {
    val hedgingPath: String = eidosConf[String]("hedgingPath")
    HypothesisHandler(hedgingPath)
  }

  // Entity Finders can be used to preload entities into the odin state, their use is optional.
  val entityFinders: Seq[Finder] = Finder.fromConfig(EidosSystem.PREFIX + ".entityFinders", config)

  val conceptExpander: ConceptExpander = {
    // Expander for expanding the bare events
    val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
    // ConceptExpander, also
    val expander: Option[Expander] = eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig)

    if (keepStatefulConcepts && expander.isEmpty)
      EidosComponents.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
    new ConceptExpander(expander, keepStatefulConcepts)
  }
}

object EidosComponents {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}