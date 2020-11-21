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

// Set all true by default.
case class EidosComponentOpts(
  // proc
  proc: Boolean = true,
  // finder
  finders: Boolean = true,
  // odin
  attachmentHandler: Boolean = true,
  conceptExpander: Boolean = true,
  corefHandler: Boolean = true,
  hedgingHandler: Boolean = true,
  mostCompleteEventsKeeper: Boolean = true,
  negationHandler: Boolean = true,
  nestedArgumentExpander: Boolean = true,
  stopwordManager: Boolean = true,
  // eidos
  adjectiveGrounder: Boolean = true,
  eidosSentenceClassifier: Boolean = true,
  ontologyHandler: Boolean = true
)

object EidosComponentOpts {
  protected lazy val cachedAll = new EidosComponentOpts()
  protected lazy val cachedNone = new EidosComponentOpts(
    false, false, false, false, false,
    false, false, false, false, false,
    false, false, false
  )

  def apply(): EidosComponentOpts = all()

  def all(): EidosComponentOpts = cachedAll

  def none(): EidosComponentOpts = cachedNone

  def proc(): EidosComponentOpts = cachedNone.copy(proc = true)

  def finders(): EidosComponentOpts = cachedNone.copy(proc = true, finders = true)

  def odin(): EidosComponentOpts = cachedAll.copy(adjectiveGrounder = false, eidosSentenceClassifier = false, ontologyHandler = false)

  def eidos(): EidosComponentOpts = cachedAll
}

case class EidosComponents(
  // proc
  procOpt: Option[EidosProcessor],
  // finder
  findersOpt: Option[Seq[Finder]],
  // odin
  attachmentHandlerOpt: Option[AttachmentHandler],
  conceptExpanderOpt: Option[ConceptExpander],
  corefHandlerOpt: Option[CorefHandler],
  hedgingHandlerOpt: Option[HypothesisHandler],
  mostCompleteEventsKeeperOpt: Option[MostCompleteEventsKeeper],
  negationHandlerOpt: Option[NegationHandler],
  nestedArgumentExpanderOpt: Option[NestedArgumentExpander],
  stopwordManagerOpt: Option[StopwordManager],
  // eidos
  adjectiveGrounderOpt: Option[AdjectiveGrounder],
  eidosSentenceClassifierOpt: Option[EidosSentenceClassifier],
  ontologyHandlerOpt: Option[OntologyHandler]
) {
  lazy val geoNormFinderOpt: Option[GeoNormFinder] = findersOpt.flatMap(_.collectFirst { case f: GeoNormFinder => f })
  lazy val useGeoNorm: Boolean = geoNormFinderOpt.isDefined

  lazy val timeNormFinderOpt: Option[TimeNormFinder] = findersOpt.flatMap(_.collectFirst { case f: TimeNormFinder => f })
  lazy val useTimeNorm: Boolean = timeNormFinderOpt.isDefined

  lazy val seasonFinderOpt: Option[SeasonFinder] = findersOpt.flatMap(_.collectFirst { case f: SeasonFinder => f })
  lazy val languageOpt: Option[String] = procOpt.map(_.language)
}

class ComponentLoader[I, T](val name: String, opt: Boolean, oldLoadedOpt: Option[T], initializer: => I, loader: I => T)
    (implicit parallel: Boolean) {
  // The lazy is so that nothing begins before the timer is started.
  protected lazy val initializedOpt: Option[Future[I]] =
      // The result of the previous initialization is not saved, so if we are reloading,
      // initialized is not recalculated.  Return None in that case.
      if (!opt || oldLoadedOpt.isDefined)
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
  lazy val loaded: Future[Option[T]] =
      if (!opt)
        Future { None }
      else
        oldLoadedOpt
            .map { oldLoaded =>
              Future {
                // If there is an old value available, use it.
                Timer.time(s"Reload $name", ComponentLoader.useTimer) {
                  Some(oldLoaded)
                }
              }
            }
            .getOrElse {
              val initialized = initializedOpt.get
              val loading = initialized.map { initialized =>
                Timer.time(s"Load $name", ComponentLoader.useTimer) {
                  Some(loader(initialized))
                }
              }
              if (!parallel) Await.result(loading, Duration.Inf) // If it isn't parallel, wait for the result.
              loading
            }

  protected def await: Option[T] = Await.result(loaded, Duration.Inf)

  // It is assumed that opt is true here.
  def get: T = await.get

  def getOpt: Option[T] = await
}

object ComponentLoader {
  var useTimer = true
}

class EidosComponentsBuilder(config: Config, eidosSystemPrefix: String, eidosComponentsOpt: Option[EidosComponents] = None,
    eidosComponentOpts: EidosComponentOpts = EidosComponentOpts(), implicit val parallel: Boolean = true) {
  Resourcer.setConfig(config) // This is a hack which initializes a global variable.

  val eidosConf: Config = config[Config](eidosSystemPrefix)
  val language = eidosConf[String]("language")
  val componentLoaders = new ArrayBuffer[ComponentLoader[_, _]]()

  val processorLoader = newComponentLoader("Processors", eidosComponentOpts.proc, eidosComponentsOpt.flatMap(_.procOpt),
    (),
    { _: Unit =>
      EidosProcessor(language, cutoff = 150)
    }
  )
  val stopwordManagerLoader = newComponentLoader("StopwordManager", eidosComponentOpts.stopwordManager, eidosComponentsOpt.flatMap(_.stopwordManagerOpt),
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      StopwordManager.fromConfig(config, tagSet)
    }
  )
  val ontologyHandlerLoader = newComponentLoader("OntologyHandler", eidosComponentOpts.ontologyHandler, eidosComponentsOpt.flatMap(_.ontologyHandlerOpt),
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
  val eidosSentenceClassifierLoader = newComponentLoader("SentenceClassifier", eidosComponentOpts.eidosSentenceClassifier, None,
    ontologyHandlerLoader.get,
    { ontologyHandler: OntologyHandler =>
      new EidosSentenceClassifier(SentenceClassifier.fromConfig(config[Config]("sentenceClassifier"), language, ontologyHandler))
    }
  )
  val processorsPrimerLoader = newComponentLoader("ProcessorsPrimer", true, None,
    processorLoader.get,
    { eidosProcessor: EidosProcessor =>
      val tokenizedDoc = eidosProcessor.mkDocument("This is a test.", keepText = true)

      if (eidosProcessor.language == Language.ENGLISH)
        eidosProcessor.annotate(tokenizedDoc)
    }
  )
  val negationHandlerLoader = newComponentLoader("NegationHandler", eidosComponentOpts.negationHandler, eidosComponentsOpt.flatMap(_.negationHandlerOpt),
    (),
    { _: Unit =>
      NegationHandler(language)
    }
  )
  val mostCompleteEventsKeeperLoader = newComponentLoader("MostCompleteEventsKeeper", eidosComponentOpts.mostCompleteEventsKeeper, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      val actions = EidosActions.fromConfig(config[Config]("actions"), tagSet)
      actions.mostCompleteEventsKeeper
    }
  )
  val hedgingHandlerLoader = newComponentLoader("HedgingHandler", eidosComponentOpts.hedgingHandler, None,
    (),
    { _: Unit =>
      HypothesisHandler(eidosConf[String]("hedgingPath"))
    }
  )
  val findersLoader = newComponentLoader("Finders", eidosComponentOpts.finders, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      Finder.fromConfig(eidosSystemPrefix + ".finders", config, tagSet)
    }
  )
  // This one is not used externally, but may benefit from parallelism.
  val expanderOptLoader = newComponentLoader("Expander", eidosComponentOpts.conceptExpander, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig(_, tagSet))
    }
  )
  val conceptExpanderLoader = newComponentLoader("ConceptExpander", eidosComponentOpts.conceptExpander, None,
    expanderOptLoader.get,
    { expanderOpt: Option[Expander] =>
      // Expander for expanding the bare events
      val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
      if (keepStatefulConcepts && expanderOpt.isEmpty)
        EidosComponentsBuilder.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
      new ConceptExpander(expanderOpt, keepStatefulConcepts)
    }
  )
  val nestedArgumentExpanderLoader = newComponentLoader("NestedArgumentExpander", eidosComponentOpts.nestedArgumentExpander, None,
    (),
    { _: Unit =>
      new NestedArgumentExpander
    }
  )
  val adjectiveGrounderLoader = newComponentLoader("AdjectiveGrounder", eidosComponentOpts.adjectiveGrounder, None,
    (),
    { _: Unit =>
      EidosAdjectiveGrounder.fromConfig(config[Config]("adjectiveGrounder"))
    }
  )
  val corefHandlerLoader = newComponentLoader("CorefHandler", eidosComponentOpts.corefHandler, None,
    (),
    { _: Unit =>
      CorefHandler.fromConfig(config[Config]("coref"))
    }
  )
  val attachmentHandlerLoader = newComponentLoader("AttachmentHandler", eidosComponentOpts.attachmentHandler, None,
    (),
    { _: Unit =>
      AttachmentHandler()
    }
  )

  def newComponentLoader[I, T](name: String, opt: Boolean, oldLoadedOpt: Option[T], initializer: => I, loader: I => T): ComponentLoader[I, T] = {
    val componentLoader = new ComponentLoader(name, opt, oldLoadedOpt, initializer, loader)
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
      // proc
      processorLoader.getOpt,
      // finders
      findersLoader.getOpt,
      // odin
      attachmentHandlerLoader.getOpt,
      conceptExpanderLoader.getOpt,
      corefHandlerLoader.getOpt,
      hedgingHandlerLoader.getOpt,
      mostCompleteEventsKeeperLoader.getOpt,
      negationHandlerLoader.getOpt,
      nestedArgumentExpanderLoader.getOpt,
      stopwordManagerLoader.getOpt,
      // eidos
      adjectiveGrounderLoader.getOpt,
      eidosSentenceClassifierLoader.getOpt,
      ontologyHandlerLoader.getOpt
    )
  }
}

object EidosComponentsBuilder {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
