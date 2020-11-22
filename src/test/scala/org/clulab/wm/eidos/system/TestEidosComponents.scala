package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.components.ComponentOpts
import org.clulab.wm.eidos.components.ComponentsBuilder
import org.clulab.wm.eidos.test.TestUtils._

class TestEidosComponents extends Test {
  val config = EidosSystem.defaultConfig

  def test(parallel: Boolean): Unit = {
    val eidosComponentsBuilder = new ComponentsBuilder(config, EidosSystem.PREFIX, None, ComponentOpts(), parallel)
    val eidosComponents = eidosComponentsBuilder.build()

    val eidosComponentsRebuilder = new ComponentsBuilder(config, EidosSystem.PREFIX, Some(eidosComponents), ComponentOpts(), parallel)
    val eidosRecomponents = eidosComponentsRebuilder.build()

    eidosRecomponents.procOpt.get                     should     be theSameInstanceAs (eidosComponents.procOpt.get)
    eidosRecomponents.negationHandlerOpt.get          should     be theSameInstanceAs (eidosComponents.negationHandlerOpt.get)
    eidosRecomponents.stopwordManagerOpt.get          should     be theSameInstanceAs (eidosComponents.stopwordManagerOpt.get)
    eidosRecomponents.ontologyHandlerOpt.get          should     be theSameInstanceAs (eidosComponents.ontologyHandlerOpt.get)
    eidosRecomponents.mostCompleteEventsKeeperOpt.get should not be theSameInstanceAs (eidosComponents.mostCompleteEventsKeeperOpt.get)
    eidosRecomponents.hedgingHandlerOpt.get           should not be theSameInstanceAs (eidosComponents.hedgingHandlerOpt.get)
    eidosRecomponents.findersOpt.get                  should not be theSameInstanceAs (eidosComponents.findersOpt.get)
    eidosRecomponents.conceptExpanderOpt.get          should not be theSameInstanceAs (eidosComponents.conceptExpanderOpt.get)
    eidosRecomponents.nestedArgumentExpanderOpt.get   should not be theSameInstanceAs (eidosComponents.nestedArgumentExpanderOpt.get)
    eidosRecomponents.adjectiveGrounderOpt.get        should not be theSameInstanceAs (eidosComponents.adjectiveGrounderOpt.get)
    eidosRecomponents.corefHandlerOpt.get             should not be theSameInstanceAs (eidosComponents.corefHandlerOpt.get)
    eidosRecomponents.attachmentHandlerOpt.get        should not be theSameInstanceAs (eidosComponents.attachmentHandlerOpt.get)
    eidosRecomponents.eidosSentenceClassifierOpt.get  should not be theSameInstanceAs (eidosComponents.eidosSentenceClassifierOpt.get)
  }

  behavior of "EidosComponents"

  // These tests take too long.
  ignore should "load in serial" in {
    test(parallel = false)
  }

  ignore should "load in parallel" in {
    test(parallel = true)
  }
}
