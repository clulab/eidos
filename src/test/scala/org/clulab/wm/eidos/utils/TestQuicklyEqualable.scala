
package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.test.TestUtils._

class TestQuicklyEqualable extends Test {

  class Superclass(val value: Int) extends QuicklyEqualable {

    override def biEquals(other: Any): Boolean = {
      val that = other.asInstanceOf[Superclass]

      this.value == that.value
    }

    override protected def calculateHashCode: Int = value
  }

  class Subclass1(value: Int) extends Superclass(value) {
  }

  class Subclass2(value: Int, var increment: Int) extends Superclass(value) {

    override def biEquals(other: Any): Boolean = {
      super.biEquals(other) && {
        val that = other.asInstanceOf[Subclass2]

        this.value == that.value &&
            this.increment == that.increment
      }
    }

    override protected def calculateHashCode: Int = super.hashCode + increment
  }

  behavior of "equals"
  
  it should "be reflexive" in {
    val one = new Subclass1(5)
    val two = one

    one == two should be (true)
  }

  it should "be reflexive with copy" in {
    val one = new Subclass1(5)
    val two = new Subclass1(5)

    one == two should be (true)
  }

  it should "be positively symmetric" in {
    val one = new Subclass1(5)
    val two = one

    ((one == two) == (two == one)) should be (true)
  }

  it should "be negatively symmetric" in {
    val one = new Subclass1(5)
    val two = new Subclass1(4)

    ((one == two) == (two == one)) should be (true)
  }

  it should "be transitive" in {
    val one = new Subclass1(5)
    val two = new Subclass1(4)
    val three = two

    if (one == two && two == three)
      one == three should be (true)
  }

  // This must be observed in the debugger.
  it should "short circuit on equals self" in {
    val one = new Subclass2(2, 3)
    val two = one

    one == two should be (true)
  }

  it should "short circuit on equals self as parent" in {
    val one = new Subclass2(2, 3)
    val two: Superclass = one
    val three = one.asInstanceOf[Superclass]

    one == two should be (true)
    one == three should be (true)
  }

  it should "not allow cross-class equality" in {
    val one = new Subclass1(5)
    val two = new Subclass2(4, 1)

    one == two should be (false)
  }

  it should "not allow a child to equal a parent" in {
    val one = new Superclass(4)
    val two = new Subclass1(4)

    one == two should be (false)
  }

  it should "be usable with an AnyVal" in {
    val one = new Subclass1(5)
    val two = 5

    one == two should be (false)
  }

  it should "equal for superclass aliases" in {
    val one = new Superclass(5)
    val two = one

    one == two should be (true)
  }

  it should "unequal for QuicklyEqualable copies" in {
    class ConcreteQuicklyEqualable extends QuicklyEqualable

    val one = new ConcreteQuicklyEqualable
    val two = new ConcreteQuicklyEqualable

    one == two should be (false)
  }
}
