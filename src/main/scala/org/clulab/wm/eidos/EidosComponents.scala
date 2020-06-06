package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.ExtractorEngine
import org.clulab.wm.eidos.EidosProcessor.EidosProcessor
import org.clulab.wm.eidos.actions.MigrationHandler
import org.clulab.wm.eidos.attachments.HypothesisHandler
import org.clulab.wm.eidos.attachments.NegationHandler
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.expansion.MostCompleteEventsKeeper
import org.clulab.wm.eidos.expansion.NestedArgumentExpander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings.AdjectiveGrounder
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.wm.eidos.utils.Timer

import org.slf4j.Logger
import org.slf4j.LoggerFactory

case class EidosComponents(
  proc: EidosProcessor,
  negationHandler: NegationHandler,
  migrationHandler: MigrationHandler,
  stopwordManager: StopwordManager,
  ontologyHandler: OntologyHandler,
  mostCompleteEventsKeeper: MostCompleteEventsKeeper,
  hedgingHandler: HypothesisHandler,
  finders: Seq[Finder],
  conceptExpander: ConceptExpander,
  nestedArgumentExpander: NestedArgumentExpander,
  adjectiveGrounder: AdjectiveGrounder
) {
  lazy val geoNormFinderOpt: Option[GeoNormFinder] = finders.collectFirst { case f: GeoNormFinder => f }
  lazy val useGeoNorm: Boolean = geoNormFinderOpt.isDefined
  lazy val timeNormFinderOpt: Option[TimeNormFinder] = finders.collectFirst { case f: TimeNormFinder => f }
  lazy val useTimeNorm: Boolean = timeNormFinderOpt.isDefined
  lazy val language: String = proc.language
}

class ComponentLoader(val name: String, loader: => Unit) {
  // This is just here to shorten definitions of loader in constructor calls.
  // "(Unit) =>" is no longer required there.  The raw block can be used.
  def load(): Unit = loader
}

class EidosComponentsBuilder(eidosSystemPrefix: String) {
  var procOpt: Option[EidosProcessor] = None
  var negationHandlerOpt: Option[NegationHandler] = None
  var migrationHandlerOpt: Option[MigrationHandler] = None
  var stopwordManagerOpt: Option[StopwordManager] = None
  var mostCompleteEventsKeeperOpt: Option[MostCompleteEventsKeeper] = None
  var ontologyHandlerOpt: Option[OntologyHandler] = None
  var hedgingHandlerOpt: Option[HypothesisHandler] = None
  var findersOpt: Option[Seq[Finder]] = None
  var conceptExpanderOpt: Option[ConceptExpander] = None
  var nestedArgumentExpanderOpt: Option[NestedArgumentExpander] = None
  var adjectiveGrounderOpt: Option[AdjectiveGrounder] = None

  var useTimer = false

  def loadComponents(componentLoaders: Seq[ComponentLoader]): Unit = {
    Timer.time("Complete parallel load", useTimer) {
      componentLoaders.par.foreach { componentLoader =>
        Timer.time("Load " + componentLoader.name, useTimer) {
          componentLoader.load()
        }
      }
    }
  }

  def add(config: Config): Unit = add(config, None)

  def add(config: Config, eidosComponents: EidosComponents): EidosComponentsBuilder =
      add(config, Some(eidosComponents))

  def add(config: Config, eidosComponentsOpt: Option[EidosComponents]): EidosComponentsBuilder = {
    val reloading: Boolean = eidosComponentsOpt.isDefined

    EidosComponentsBuilder.logger.info((if (reloading) "Reloading" else "Loading") + " config...")

    val eidosConf: Config = config[Config](eidosSystemPrefix)

    val headComponentLoaders: Seq[ComponentLoader] = if (reloading) {
      // When reloading, the expensive things and those required to make them are borrowed from previous components.
      val eidosComponents = eidosComponentsOpt.get

      procOpt = Some(eidosComponents.proc)
      stopwordManagerOpt = Some(eidosComponents.stopwordManager)
      // This involves reloading of very large vector files and is what we're trying to avoid.
      ontologyHandlerOpt = Some(eidosComponents.ontologyHandler)
      // This only depends on language, not any settings, so a reload is safe.
      negationHandlerOpt = Some(eidosComponents.negationHandler)
      Seq.empty
    }
    else {
      val language = eidosConf[String]("language")
      val preComponentLoaders = Seq(
        new ComponentLoader("Processors", {
          EidosComponentsBuilder.logger.info("Loading processor...")

          procOpt = Some(EidosProcessor(language, cutoff = 150))
        })
      )
      // Get these out of the way so that the ontologyHandler can take its time about it
      // even while the processor is busy priming.
      loadComponents(preComponentLoaders)

      val tagSet = procOpt.get.getTagSet
      val postpreComponentLoaders = Seq(
        new ComponentLoader("StopwordManager", { stopwordManagerOpt = Some(StopwordManager.fromConfig(config, tagSet)) }),
      )
      loadComponents(postpreComponentLoaders)

      Seq(
        new ComponentLoader("OntologyHandler", {
          ontologyHandlerOpt = Some(OntologyHandler.load(config[Config]("ontologies"), procOpt.get, stopwordManagerOpt.get, tagSet))
        }),
        new ComponentLoader("ProcessorsPrimer", {
          val eidosProcessor = procOpt.get
          val tokenizedDoc = eidosProcessor.mkDocument("This is a test.", keepText = true)

          if (eidosProcessor.language == "english")
            eidosProcessor.annotate(tokenizedDoc)
        }),
        new ComponentLoader("NegationHandler", { negationHandlerOpt = Some(NegationHandler(language)) })
      )
    }

    val tailComponentLoaders = Seq(
      new ComponentLoader("MigrationHandler", { migrationHandlerOpt = Some(MigrationHandler()) }),
      new ComponentLoader("MostCompleteEventsKeeper", {
        val actions = EidosActions.fromConfig(config[Config]("actions"), procOpt.get.getTagSet)

        mostCompleteEventsKeeperOpt = Some(actions.mostCompleteEventsKeeper)
      }),
      new ComponentLoader("HedgingHandler", { hedgingHandlerOpt = Some(HypothesisHandler(eidosConf[String]("hedgingPath"))) }),
      // Extraction is performed using a sequence of finders.
      new ComponentLoader("Finders", { findersOpt = Some(Finder.fromConfig(eidosSystemPrefix + ".finders", config, procOpt.get.getTagSet)) }),
      new ComponentLoader("ConceptExpander", { conceptExpanderOpt = {
        // Expander for expanding the bare events
        val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
        // ConceptExpander, also
        val expander: Option[Expander] = eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig(_, procOpt.get.getTagSet))

        if (keepStatefulConcepts && expander.isEmpty)
          EidosComponentsBuilder.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
        Some(new ConceptExpander(expander, keepStatefulConcepts))
      } }),
      new ComponentLoader("NestedArgumentExpander", { nestedArgumentExpanderOpt = Some(new NestedArgumentExpander) }),
      new ComponentLoader("AdjectiveGrounder", { adjectiveGrounderOpt = Some(EidosAdjectiveGrounder.fromConfig(config[Config]("adjectiveGrounder"))) })
    )
    val componentLoaders = headComponentLoaders ++ tailComponentLoaders
    loadComponents(componentLoaders)
    this
  }

  def build(): EidosComponents = {
    EidosComponents(
      procOpt.get,
      negationHandlerOpt.get,
      migrationHandlerOpt.get,
      stopwordManagerOpt.get,
      ontologyHandlerOpt.get,
      mostCompleteEventsKeeperOpt.get,
      hedgingHandlerOpt.get,
      findersOpt.get,
      conceptExpanderOpt.get,
      nestedArgumentExpanderOpt.get,
      adjectiveGrounderOpt.get
    )
  }
}

object EidosComponentsBuilder {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
