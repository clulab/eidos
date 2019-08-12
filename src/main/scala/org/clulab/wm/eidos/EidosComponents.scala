package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.Processor
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.utils._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

case class EidosComponents(
  proc: Processor,
  negationHandler: NegationHandler,
  documentFilter: DocumentFilter,
  stopwordManager: StopwordManager,
  ontologyHandler: OntologyHandler,
  multiOntologyGrounder: MultiOntologyGrounder,
  actions: EidosActions,
  engine: ExtractorEngine,
  hedgingHandler: HypothesisHandler,
  entityFinders: Seq[Finder],
  conceptExpander: ConceptExpander
) {
  lazy val geoNormFinderOpt: Option[GeoNormFinder] = entityFinders.collectFirst { case f: GeoNormFinder => f }
  lazy val useGeoNorm: Boolean = geoNormFinderOpt.isDefined
  lazy val timeNormFinderOpt: Option[TimeNormFinder] = entityFinders.collectFirst { case f: TimeNormFinder => f }
  lazy val useTimeNorm: Boolean = timeNormFinderOpt.isDefined
  lazy val language = negationHandler.language
}

class EidosComponentsBuilder {
  var procOpt: Option[Processor] = None
  var negationHandlerOpt: Option[NegationHandler] = None
  var documentFilterOpt: Option[DocumentFilter] = None
  var stopwordManagerOpt: Option[StopwordManager] = None
  var ontologyHandlerOpt: Option[OntologyHandler] = None
  var multiOntologyGrounderOpt: Option[MultiOntologyGrounder] = None
  var actionsOpt: Option[EidosActions] = None
  var engineOpt: Option[ExtractorEngine] = None
  var hedgingHandlerOpt: Option[HypothesisHandler] = None
  var entityFindersOpt: Option[Seq[Finder]] = None
  var conceptExpanderOpt: Option[ConceptExpander] = None

  def add(config: Config): Unit = add(config, None)

  def add(config: Config, eidosComponents: EidosComponents): EidosComponentsBuilder =
      add(config, Some(eidosComponents))

  def add(config: Config, eidosComponentsOpt: Option[EidosComponents]): EidosComponentsBuilder = {
    val reloading: Boolean = eidosComponentsOpt.isDefined

    EidosComponentsBuilder.logger.info((if (reloading) "Reloading" else "Loading") + " config...")

    val eidosConf: Config = config[Config](EidosSystem.PREFIX)

    if (reloading) {
      // When reloading, the expensive things and those required to make them are borrowed from previous components.
      val eidosComponents = eidosComponentsOpt.get

      procOpt = Some(eidosComponents.proc)
      // This only depends on language, not any settings, so a reload is safe.
      negationHandlerOpt = Some(eidosComponents.negationHandler)
      // This is only dependent on proc, which is not reloaded, so don't reload here.
      documentFilterOpt = Some(eidosComponents.documentFilter)
      stopwordManagerOpt = Some(eidosComponents.stopwordManager)
      // This involves reloading of very large vector files and is what we're trying to avoid.
      ontologyHandlerOpt = Some(eidosComponents.ontologyHandler)
      multiOntologyGrounderOpt = Some(eidosComponents.multiOntologyGrounder)
    }
    else {
      {
        val language = eidosConf[String]("language")
        EidosComponentsBuilder.logger.info("Loading processor...")

        procOpt = Some(language match {
          case "english" => new FastNLPProcessor
          case "spanish" => new SpanishCluProcessor
          case "portuguese" => new PortugueseCluProcessor
        })
        // This is the only record of the language.  The Processor cannot provide it.
        negationHandlerOpt = Some(NegationHandler(language))
      }
      // Prunes sentences form the Documents to reduce noise/allow reasonable processing time
      documentFilterOpt = Some(FilterByLength(procOpt.get, cutoff = 150))
      stopwordManagerOpt = Some(StopwordManager.fromConfig(config))
      ontologyHandlerOpt = Some(OntologyHandler.load(config[Config]("ontologies"), procOpt.get, stopwordManagerOpt.get))
      // Ontologies
      multiOntologyGrounderOpt = Some(ontologyHandlerOpt.get.ontologyGrounders)
    }

    actionsOpt = Some(EidosActions.fromConfig(config[Config]("actions")))
    engineOpt = { // ODIN component
      val masterRulesPath: String = eidosConf[String]("masterRulesPath")
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)

      Some(ExtractorEngine(masterRules, actionsOpt.get, actionsOpt.get.globalAction))
    }

    // This seems to be unused
    // protected val taxonomyPath: String = eidosConf[String]("taxonomyPath")

    // Hedging
    hedgingHandlerOpt = {
      val hedgingPath: String = eidosConf[String]("hedgingPath")

      Some(HypothesisHandler(hedgingPath))
    }

    // Entity Finders can be used to preload entities into the odin state, their use is optional.
    entityFindersOpt = Some(Finder.fromConfig(EidosSystem.PREFIX + ".entityFinders", config))
    conceptExpanderOpt = {
      // Expander for expanding the bare events
      val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
      // ConceptExpander, also
      val expander: Option[Expander] = eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig)

      if (keepStatefulConcepts && expander.isEmpty)
        EidosComponentsBuilder.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
      Some(new ConceptExpander(expander, keepStatefulConcepts))
    }
    this
  }

  def build(): EidosComponents = {
    EidosComponents(
      procOpt.get,
      negationHandlerOpt.get,
      documentFilterOpt.get,
      stopwordManagerOpt.get,
      ontologyHandlerOpt.get,
      multiOntologyGrounderOpt.get,
      actionsOpt.get,
      engineOpt.get,
      hedgingHandlerOpt.get,
      entityFindersOpt.get,
      conceptExpanderOpt.get
    )
  }
}

object EidosComponentsBuilder {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
