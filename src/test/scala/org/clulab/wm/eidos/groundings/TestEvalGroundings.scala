package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.apps.EvalGroundings
import org.clulab.wm.eidos.test.TestUtils.EidosTest

class TestEvalGroundings extends EidosTest {

  behavior of "grounding algorithm"

  // This test is generally ignored because it takes a very long time to run.
  // Change "ignore" to "it" on a local machine and run locally to spot check.
  ignore should "not degrade in performance" in {
    val expectedResults = Map(
      "wm_flattened"              -> 0.7135416f,
      "wm_compositional/concept"  -> 0.5197916f,
      "wm_compositional/process"  -> 0.45f,
      "wm_compositional/property" -> 0.7875f
    )
    val actualResults = EvalGroundings
        .test()
        .map { evaluator => evaluator.grounderName -> evaluator.getAccuracy }
        .toMap

    expectedResults.foreach { case (expectedName, expectedScore) =>
      actualResults should contain key (expectedName)
      // This test is fairly harsh and we'll probably back down from it.
      actualResults(expectedName) should be >= expectedScore
    }
  }
}
