package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.EidosComponentsBuilder
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils._

class TestEidosComponents extends Test {
  val config = EidosSystem.defaultConfig

  def test(parallel: Boolean): Unit = {
    val eidosComponentsBuilder = new EidosComponentsBuilder(config, EidosSystem.PREFIX, None, parallel)
    val eidosComponents = eidosComponentsBuilder.build()

    val eidosComponentsRebuilder = new EidosComponentsBuilder(config, EidosSystem.PREFIX, Some(eidosComponents), parallel)
    val eidosRecomponents = eidosComponentsRebuilder.build()

    eidosRecomponents.proc                     should     be theSameInstanceAs (eidosComponents.proc)
    eidosRecomponents.negationHandler          should     be theSameInstanceAs (eidosComponents.negationHandler)
    eidosRecomponents.stopwordManager          should     be theSameInstanceAs (eidosComponents.stopwordManager)
    eidosRecomponents.ontologyHandler          should     be theSameInstanceAs (eidosComponents.ontologyHandler)
    eidosRecomponents.mostCompleteEventsKeeper should not be theSameInstanceAs (eidosComponents.mostCompleteEventsKeeper)
    eidosRecomponents.hedgingHandler           should not be theSameInstanceAs (eidosComponents.hedgingHandler)
    eidosRecomponents.finders                  should not be theSameInstanceAs (eidosComponents.finders)
    eidosRecomponents.conceptExpander          should not be theSameInstanceAs (eidosComponents.conceptExpander)
    eidosRecomponents.nestedArgumentExpander   should not be theSameInstanceAs (eidosComponents.nestedArgumentExpander)
    eidosRecomponents.adjectiveGrounder        should not be theSameInstanceAs (eidosComponents.adjectiveGrounder)
    eidosRecomponents.corefHandler             should not be theSameInstanceAs (eidosComponents.corefHandler)
    eidosRecomponents.attachmentHandler        should not be theSameInstanceAs (eidosComponents.attachmentHandler)
    eidosRecomponents.eidosSentenceClassifier  should not be theSameInstanceAs (eidosComponents.eidosSentenceClassifier)
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
