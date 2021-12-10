package org.clulab.wm.eidoscommon.utils

class TestIdentityHashMap extends Test {

  behavior of "IdentityHashMap"

  it should "not find values that differ" in {
    val holder42 = new IntHolder(42)
    val holder13 = new IntHolder(13)
    val identityHashMap = IdentityHashMap[IntHolder, Int]()

    identityHashMap += holder42 -> holder42.value

    identityHashMap.get(holder13) should be (None)
  }

  it should "not find values that are == but not eq" in {
    val holder42a = new IntHolder(42)
    val holder42b = new IntHolder(42)
    val identityHashMap = IdentityHashMap[IntHolder, Int]()

    identityHashMap += holder42a -> holder42a.value

    identityHashMap.get(holder42b) should be (None)
  }

  it should "not find values that reference the same object" in {
    val holder42a = new IntHolder(42)
    val holder42b = holder42a
    val identityHashMap = IdentityHashMap[IntHolder, Int]()

    identityHashMap += holder42a -> holder42a.value

    identityHashMap.get(holder42b) should be (Some(holder42a.value))
  }
}
