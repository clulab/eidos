package org.clulab.wm.eidoscommon.utils

class TestIdentityHashBag extends Test {

  behavior of "IdentityHashBag"

  it should "not find values that differ" in {
    val holder42 = new IntHolder(42)
    val holder13 = new IntHolder(13)
    val identityHashBag = IdentityHashBag[IntHolder]()

    identityHashBag += holder42

    identityHashBag(holder13) should be (false)
  }

  it should "not find values that are == but not eq" in {
    val holder42a = new IntHolder(42)
    val holder42b = new IntHolder(42)
    val identityHashBag = IdentityHashBag[IntHolder]()

    identityHashBag += holder42a

    identityHashBag(holder42b) should be (false)
  }

  it should "not find values that reference the same object" in {
    val holder42a = new IntHolder(42)
    val holder42b = holder42a
    val identityHashBag = IdentityHashBag[IntHolder]

    identityHashBag += holder42a

    identityHashBag(holder42b) should be (true)
  }
}
