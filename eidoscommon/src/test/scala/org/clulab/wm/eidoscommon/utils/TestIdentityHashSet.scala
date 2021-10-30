package org.clulab.wm.eidoscommon.utils

class TestIdentityHashSet extends Test {

  class IntHolder(val value: Int) {

    override def equals(other: Any): Boolean =
        this.value == other.asInstanceOf[IntHolder].value

    override def hashCode: Int = value
  }

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
