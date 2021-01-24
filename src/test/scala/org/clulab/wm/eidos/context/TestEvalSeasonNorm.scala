package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.apps.EvalSeasonNorm
import org.clulab.wm.eidos.test.TestUtils.Test

class TestEvalSeasonNorm extends Test {

  behavior of "temporal parser"

  // This test is generally ignored because it takes a very long time to run.
  // Change "ignore" to "it" on a local machine and run locally to spot check.
  it should "not degrade in performance" in {
    val expectedFscore = 0.67
    val actualFscore = EvalSeasonNorm.test()
    actualFscore should be >= expectedFscore
  }
}
