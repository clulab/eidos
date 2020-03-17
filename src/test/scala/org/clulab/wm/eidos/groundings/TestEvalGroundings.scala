package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.apps.EvalGroundings
import org.clulab.wm.eidos.test.TestUtils.Test

class TestEvalGroundings extends Test {

  behavior of "grounding algorithm"

  it should "not degrade in performance" in {
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
      actualResults should contain (expectedName)
      actualResults(expectedName) should be >= expectedScore
    }
  }
}
