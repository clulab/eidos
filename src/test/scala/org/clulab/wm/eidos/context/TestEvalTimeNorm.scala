package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.apps.EvalTimeNorm
import org.clulab.wm.eidos.test.EidosTest

class TestEvalTimeNorm extends EidosTest {

  behavior of "temporal parser"

  // This test is generally ignored because it takes a very long time to run.
  // Change "ignore" to "it" on a local machine and run locally to spot check.
  longTimeNormTest should "not degrade in performance" in {
    val expectedFscore = 0.67
    val actualFscore = EvalTimeNorm.test()
    actualFscore should be >= expectedFscore
  }
}
