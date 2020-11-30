package org.clulab.wm.eidos.components

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.attachments.AttachmentHandler
import org.clulab.wm.eidos.attachments.HypothesisHandler
import org.clulab.wm.eidos.attachments.NegationHandler
import org.clulab.wm.eidos.components
import org.clulab.wm.eidos.document.EidosSentenceClassifier
import org.clulab.wm.eidos.document.SentenceClassifier
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.expansion.NestedArgumentExpander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.groundings.grounders.EidosAdjectiveGrounder
import org.clulab.wm.eidos.utils.Domain
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.wm.eidoscommon.EidosProcessor
import org.clulab.wm.eidoscommon.Language
import org.clulab.wm.eidoscommon.TagSet
import org.clulab.wm.eidoscommon.utils.Resourcer
import org.clulab.wm.eidoscommon.utils.Timer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class ComponentsBuilder(config: Config, eidosSystemPrefix: String, eidosComponentsOpt: Option[EidosComponents] = None,
    componentOpts: ComponentOpts = ComponentOpts(), implicit val parallel: Boolean = true) {
  import scala.concurrent.ExecutionContext.Implicits.global

  val eidosConf: Config = config[Config](eidosSystemPrefix)
  val language: String = eidosConf[String]("language")
  val componentLoaders = new ArrayBuffer[ComponentLoader[_, _]]()

  Resourcer.setConfig(config) // This is a hack which initializes a global variable.
  Domain.setConfig(config) // likewise

  val processorLoader = newComponentLoader("Processors", componentOpts.proc, eidosComponentsOpt.flatMap(_.procOpt),
    (),
    { _: Unit =>
      EidosProcessor(language, cutoff = 150)
    }
  )
  val stopwordManagerLoader = newComponentLoader("StopwordManager", componentOpts.stopwordManager, eidosComponentsOpt.flatMap(_.stopwordManagerOpt),
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      StopwordManager.fromConfig(config, tagSet)
    }
  )
  val ontologyHandlerLoader = newComponentLoader("OntologyHandler", componentOpts.ontologyHandler, eidosComponentsOpt.flatMap(_.ontologyHandlerOpt),
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
  val eidosSentenceClassifierLoader = newComponentLoader("SentenceClassifier", componentOpts.eidosSentenceClassifier, None,
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
  val negationHandlerLoader = newComponentLoader("NegationHandler", componentOpts.negationHandler, eidosComponentsOpt.flatMap(_.negationHandlerOpt),
    (),
    { _: Unit =>
      NegationHandler(language)
    }
  )
  val mostCompleteEventsKeeperLoader = newComponentLoader("MostCompleteEventsKeeper", componentOpts.mostCompleteEventsKeeper, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      val actions = EidosActions.fromConfig(config[Config]("actions"), tagSet)
      actions.mostCompleteEventsKeeper
    }
  )
  val hedgingHandlerLoader = newComponentLoader("HedgingHandler", componentOpts.hedgingHandler, None,
    (),
    { _: Unit =>
      HypothesisHandler(eidosConf[String]("hedgingPath"))
    }
  )
  val findersLoader = newComponentLoader("Finders", componentOpts.finders, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      Finder.fromConfig(eidosSystemPrefix + ".finders", config, tagSet)
    }
  )
  // This one is not used externally, but may benefit from parallelism.
  val expanderOptLoader = newComponentLoader("Expander", componentOpts.conceptExpander, None,
    processorLoader.get.getTagSet,
    { tagSet: TagSet =>
      eidosConf.get[Config]("conceptExpander").map(Expander.fromConfig(_, tagSet))
    }
  )
  val conceptExpanderLoader = newComponentLoader("ConceptExpander", componentOpts.conceptExpander, None,
    expanderOptLoader.get,
    { expanderOpt: Option[Expander] =>
      // Expander for expanding the bare events
      val keepStatefulConcepts: Boolean = eidosConf[Boolean]("keepStatefulConcepts")
      if (keepStatefulConcepts && expanderOpt.isEmpty)
        ComponentsBuilder.logger.warn("You're keeping stateful Concepts but didn't load an expander.")
      new ConceptExpander(expanderOpt, keepStatefulConcepts)
    }
  )
  val nestedArgumentExpanderLoader = newComponentLoader("NestedArgumentExpander", componentOpts.nestedArgumentExpander, None,
    (),
    { _: Unit =>
      new NestedArgumentExpander
    }
  )
  val adjectiveGrounderLoader = newComponentLoader("AdjectiveGrounder", componentOpts.adjectiveGrounder, None,
    (),
    { _: Unit =>
      EidosAdjectiveGrounder.fromConfig(config[Config]("adjectiveGrounder"))
    }
  )
  val corefHandlerLoader = newComponentLoader("CorefHandler", componentOpts.corefHandler, None,
    (),
    { _: Unit =>
      CorefHandler.fromConfig(config[Config]("coref"))
    }
  )
  val attachmentHandlerLoader = newComponentLoader("AttachmentHandler", componentOpts.attachmentHandler, None,
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
    components.EidosComponents(
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

object ComponentsBuilder {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
}