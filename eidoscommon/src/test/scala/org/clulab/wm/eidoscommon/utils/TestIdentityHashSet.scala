package org.clulab.wm.eidoscommon.utils

class TestIdentityHashSet extends Test {

  behavior of "IdentityHashSet"

  it should "not find values that differ" in {
    val holder42 = new IntHolder(42)
    val holder13 = new IntHolder(13)
    val identityHashSet = IdentityHashSet[IntHolder]()

    identityHashSet += holder42

    identityHashSet(holder13) should be (false)
  }

  it should "not find values that are == but not eq" in {
    val holder42a = new IntHolder(42)
    val holder42b = new IntHolder(42)
    val identityHashSet = IdentityHashSet[IntHolder]()

    identityHashSet += holder42a

    identityHashSet(holder42b) should be (false)
  }

  it should "not find values that reference the same object" in {
    val holder42a = new IntHolder(42)
    val holder42b = holder42a
    val identityHashSet = IdentityHashSet[IntHolder]

    identityHashSet += holder42a

    identityHashSet(holder42b) should be (true)
  }
}
