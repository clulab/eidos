package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.apps.eval.EvalSeasonNorm
import org.clulab.wm.eidos.test.EidosTest

class TestEvalSeasonNorm extends EidosTest {

  behavior of "temporal parser"

  // This test is generally ignored because it takes a very long time to run.
  // Change "ignore" to "it" on a local machine and run locally to spot check.
  longTimeNormTest should "not degrade in performance" in {
    val expectedFscore = 0.46
    val actualFscore = EvalSeasonNorm.test()
    actualFscore should be >= expectedFscore
  }
}
