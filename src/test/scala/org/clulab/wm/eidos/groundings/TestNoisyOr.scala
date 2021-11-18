package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.GroundingUtils
import org.clulab.wm.eidoscommon.utils.Test

class TestNoisyOr extends Test {

  behavior of "NoisyOr"

  ignore should "always get results within range" in {
    val result = GroundingUtils.noisyOr(Seq(0f, 0f, 0f))

    result should be <= 1f
    result should be >= 0f
  }
}
