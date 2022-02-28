package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.GroundingUtils
import org.clulab.wm.eidoscommon.utils.Test

class TestNoisyOr extends Test {

  behavior of "NoisyOr"

  // This is known to fail!
  ignore should "always get results within range" in {
    val result = GroundingUtils.noisyOr(Seq(0f, 0f, 0f))

    result should be <= 1f
    result should be >= 0f
  }

  it should "not let 1 degrade quickly to 0" in {
    val result = GroundingUtils.noisyOr(Seq(1f, 0f, 0f, 0f))

    result should be > 0f
  }

  it should "quickly increase from 0 to 1" in {
    val result = GroundingUtils.noisyOr(Seq(0f, 1f))

    result should be > 0f
  }

  it should "give a higher score when there are more items" in {
    val result1 = GroundingUtils.noisyOr(Seq(1f))
    val result2 = GroundingUtils.noisyOr(Seq(1f, 1f))

    result2 should be > result1
  }

  it should "get zero for empty" in {
    val result = GroundingUtils.noisyOr(Seq.empty[Float])

    result should be (0.0f)
  }
}
