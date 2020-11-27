package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosProcessor.EidosProcessor
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.attachments.{AttachmentHandler, HypothesisHandler, NegationHandler}
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.context.SeasonFinder
import org.clulab.wm.eidos.document.EidosSentenceClassifier
import org.clulab.wm.eidos.document.SentenceClassifier
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.expansion.MostCompleteEventsKeeper
import org.clulab.wm.eidos.expansion.NestedArgumentExpander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings.AdjectiveGrounder
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.utils.Domain
import org.clulab.wm.eidos.utils.Language
import org.clulab.wm.eidos.utils.Resourcer
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.wm.eidos.utils.TagSet
import org.clulab.wm.eidos.utils.Timer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

case class EidosComponents(
  proc: EidosProcessor,
  negationHandler: NegationHandler,
  stopwordManager: StopwordManager,
  ontologyHandler: OntologyHandler,
  mostCompleteEventsKeeper: MostCompleteEventsKeeper,
  hedgingHandler: HypothesisHandler,
  finders: Seq[Finder],
  conceptExpander: ConceptExpander,
  nestedArgumentExpander: NestedArgumentExpander,
  adjectiveGrounder: AdjectiveGrounder,
  corefHandler: CorefHandler,
  attachmentHandler: AttachmentHandler,
  eidosSentenceClassifier: EidosSentenceClassifier
) {
  lazy val geoNormFinderOpt: Option[GeoNormFinder] = finders.collectFirst { case f: GeoNormFinder => f }
  lazy val useGeoNorm: Boolean = geoNormFinderOpt.isDefined
  lazy val timeNormFinderOpt: Option[TimeNormFinder] = finders.collectFirst { case f: TimeNormFinder => f }
  lazy val seasonFinderOpt: Option[SeasonFinder] = finders.collectFirst { case f: SeasonFinder => f }
  lazy val useTimeNorm: Boolean = timeNormFinderOpt.isDefined
  lazy val language: String = proc.language
}

class ComponentLoader[I, T](val name: String, oldLoadedOpt: Option[T], initializer: => I, loader: I => T)
    (implicit parallel: Boolean) {
  // The lazy is so that nothing begins before the timer is started.
  protected lazy val initializedOpt: Option[Future[I]] =
      // The result of the previous initialization is not saved, so if we are reloading,
      // initialized is not recalculated.  Return None in that case.
      if (oldLoadedOpt.isDefined)
        None
      else {
        val initializing = Future {
          Timer.time(s"Init $name", ComponentLoader.useTimer) {
            initializer
          }
        }
        if (!parallel) Await.result(initializing, Duration.Inf) // If it isn't parallel, wait for the result.
        Some(initializing)
      }
  lazy val loaded: Future[T] = oldLoadedOpt
      .map { oldLoaded =>
        Future {
          // If there is an old value available, use it.
          Timer.time(s"Reload $name", ComponentLoader.useTimer) {
            oldLoaded
          }
        }
      }
      .getOrElse {
        val initialized = initializedOpt.get
        val loading = initialized.map { initialized =>
          Timer.time(s"Load $name", ComponentLoader.useTimer) {
            loader(initialized)
          }
        }
        if (!parallel) Await.result(loading, Duration.Inf) // If it isn't parallel, wait for the result.
        loading
      }

  def get: T = Await.result(loaded, Duration.Inf)
}

object ComponentLoader {
  var useTimer = true
}

class EidosComponentsBuilder(config: Config, eidosSystemPrefix: String, eidosComponentsOpt: Option[EidosComponents] = None,
    implicit val parallel: Boolean = true) {  
  val eidosConf: Config = config[Config](eidosSystemPrefix)
  val language = eidosConf[String]("language")
  val componentLoaders = new ArrayBuffer[ComponentLoader[_, _]]()

  Resourcer.setConfig(config) // This is a hack which initializes a global variable.
  Domain.setConfig(config) // likewise

  val processorLoader = newComponentLoader("Processors", eidosComponentsOpt.map(_.proc),
    (),
    { _: Unit =>
      EidosProcessor(language, cutoff = 150)
    }
  )
  val stopwordManagerLoader = newComponentLoader("StopwordManager", eidosComponentsOpt.map(_.stopwordManager),
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      StopwordManager.fromConfig(config, tagSet)
    }
  )
  val ontologyHandlerLoader = newComponentLoader("OntologyHandler", eidosComponentsOpt.map(_.ontologyHandler),
    {
      // Make sure these are both being worked on before any waiting is done.
      (processorLoader.loaded, stopwordManagerLoader.loaded)
      (processorLoader.get, stopwordManagerLoader.get)
    },
    { processorAndStopwordManager: (EidosProcessor, StopwordManager) =>
      val (processor, stopwordManager) = processorAndStopwordManager
      OntologyHandler.load(config[Config]("ontologies"), processor, stopwordManager, processor.getTagSet, processor.getTokenizer)
    }
  )
  val eidosSentenceClassifierLoader = newComponentLoader("SentenceClassifier", None,
    ontologyHandlerLoader.get,
    { ontologyHandler: OntologyHandler =>
      new EidosSentenceClassifier(SentenceClassifier.fromConfig(config[Config]("sentenceClassifier"), language, ontologyHandler))
    }
  )
  val processorsPrimerLoader = newComponentLoader("ProcessorsPrimer", None,
    processorLoader.get,
    { eidosProcessor: EidosProcessor =>
      val tokenizedDoc = eidosProcessor.mkDocument("This is a test.", keepText = true)

      if (eidosProcessor.language == Language.ENGLISH)
        eidosProcessor.annotate(tokenizedDoc)
    }
  )
  val negationHandlerLoader = newComponentLoader("NegationHandler", eidosComponentsOpt.map(_.negationHandler),
    (),
    { _: Unit =>
      NegationHandler(language)
    }
  )
  val mostCompleteEventsKeeperLoader = newComponentLoader("MostCompleteEventsKeeper", None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      val actions = EidosActions.fromConfig(config[Config]("actions"), tagSet)
      actions.mostCompleteEventsKeeper
    }
  )
  val hedgingHandlerLoader = newComponentLoader("HedgingHandler", None,
    (),
    { _: Unit =>
      HypothesisHandler(eidosConf[String]("hedgingPath"))
    }
  )
  val findersLoader = newComponentLoader("Finders", None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      Finder.fromConfig(eidosSystemPrefix + ".finders", config, tagSet)
    }
  )
  // This one is not used externally, but may benefit from parallelism.
  val expanderOptLoader = newComponentLoader("Expander", None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig(_, tagSet))
    }
  )
  val conceptExpanderLoader = newComponentLoader("ConceptExpander", None,
    expanderOptLoader.get,
    { expanderOpt: Option[Expander] =>
      // Expander for expanding the bare events
      val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
      if (keepStatefulConcepts && expanderOpt.isEmpty)
        EidosComponentsBuilder.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
      new ConceptExpander(expanderOpt, keepStatefulConcepts)
    }
  )
  val nestedArgumentExpanderLoader = newComponentLoader("NestedArgumentExpander", None,
    (),
    { _: Unit =>
      new NestedArgumentExpander
    }
  )
  val adjectiveGrounderLoader = newComponentLoader("AdjectiveGrounder", None,
    (),
    { _: Unit =>
      EidosAdjectiveGrounder.fromConfig(config[Config]("adjectiveGrounder"))
    }
  )
  val corefHandlerLoader = newComponentLoader("CorefHandler", None,
    (),
    { _: Unit =>
      CorefHandler.fromConfig(config[Config]("coref"))
    }
  )
  val attachmentHandlerLoader = newComponentLoader("AttachmentHandler", None,
    (),
    { _: Unit =>
      AttachmentHandler()
    }
  )

  def newComponentLoader[I, T](name: String, oldLoadedOpt: Option[T], initializer: => I, loader: I => T): ComponentLoader[I, T] = {
    val componentLoader = new ComponentLoader(name, oldLoadedOpt, initializer, loader)
    componentLoaders += componentLoader
    componentLoader
  }

  def build(): EidosComponents = {
    Timer.time(s"Load components", ComponentLoader.useTimer) {
      if (parallel) {
        val futures = componentLoaders.map(_.loaded)
        val future = Future.sequence(futures)
        Await.result(future, Duration.Inf)
      }
      else
        componentLoaders.foreach(_.get)
    }
    EidosComponents(
      processorLoader.get,
      negationHandlerLoader.get,
      stopwordManagerLoader.get,
      ontologyHandlerLoader.get,
      mostCompleteEventsKeeperLoader.get,
      hedgingHandlerLoader.get,
      findersLoader.get,
      conceptExpanderLoader.get,
      nestedArgumentExpanderLoader.get,
      adjectiveGrounderLoader.get,
      corefHandlerLoader.get,
      attachmentHandlerLoader.get,
      eidosSentenceClassifierLoader.get
    )
  }
}

object EidosComponentsBuilder {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
