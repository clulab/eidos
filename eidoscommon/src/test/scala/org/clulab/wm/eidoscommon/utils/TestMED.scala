package org.clulab.wm.eidoscommon.utils

class TestMED extends Test {
  behavior of "MED"

  it should "calculate distance correctly" in {
    MED("cat", "cars").getDistance should be (3)
    MED("kitten", "sitting").getDistance should be (5)
    MED("Sunday", "Saturday").getDistance should be (4)
  }
}
