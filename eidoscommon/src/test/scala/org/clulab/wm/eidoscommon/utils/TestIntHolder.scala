package org.clulab.wm.eidoscommon.utils

class TestIntHolder extends Test {

  behavior of "IntHolder"

  it should "not be == or eq if values differ" in {
    val holder42 = new IntHolder(42)
    val holder13 = new IntHolder(13)

    holder42 == holder13 should be (false)
    holder42 eq holder13 should be (false)
  }

  it should "be == but not eq if values are the same" in {
    val holder42a = new IntHolder(42)
    val holder42b = new IntHolder(42)

    holder42a == holder42b should be (true)
    holder42a eq holder42b should be (false)
  }

  it should "not be == or eq if values reference the same object" in {
    val holder42a = new IntHolder(42)
    val holder42b = holder42a

    holder42a == holder42b should be (true)
    holder42a eq holder42b should be (true)
  }
}
