
package org.clulab.wm.eidos.util

import org.clulab.wm.eidos.test.TestUtils._

class TestEquals extends Test {

  /*
  trait QuicklyIdentifiable {
    def canEqual(other: AnyRef): Boolean = this.getClass() == other.getClass()

    protected def triEquals(other: Any): Option[Boolean] = {
      // The order of these first few tests will have a slight influence on efficiency.
      if (!other.isInstanceOf[AnyRef])
        Some(false) // Don't allow an AnyRef (this) to equal an AnyVal.
      else {
        val anyOther = other.asInstanceOf[AnyRef]

        if (!this.canEqual(anyOther))
          Some(false)
        else if (this.eq(anyOther))
          Some(true)
        else if (this.hashCode != other.hashCode)
          Some(false)
        else None
      } // Leave it up to equals.
      // Is it necessary to do that.canEqual(this)?
      // Not if they are both the exact same class.
    }

    // This could be very expensive.  This is default behavior so that
    // subclasses do not necessarily have to override.  Equality is eq.
    override def equals(other: Any): Boolean = {
      triEquals(other).getOrElse(false)
    }

    // This could be expensive, so therefore cache it.
    override def hashCode: Int = cachedHashCode

    protected lazy val cachedHashCode = calculateHashCode

    protected def calculateHashCode: Int = 0
  }
*/

  // The hash and equals functions may be time consuming, so why not check for eq as well?
  class Superclass(val value: Int) {
    // Default implementation so that subclasses do not have to override
    def canEqual(other: AnyRef): Boolean = this.getClass() == other.getClass()

    protected def triEquals(other: Any): Option[Boolean] = {
      // The order of these first few tests will have a slight influence on efficiency.
      if (!other.isInstanceOf[AnyRef])
        Some(false) // Don't allow an AnyRef (this) to equal an AnyVal.
      else {
        val anyOther = other.asInstanceOf[AnyRef]

        if (!this.canEqual(anyOther))
          Some(false)
        else if (this.eq(anyOther))
          Some(true)
        else if (this.hashCode != other.hashCode)
          Some(false)
        else None
      } // Leave it up to equals.
      // Is it necessary to do that.canEqual(this)?
      // Not if they are both the exact same class.
    }

    // This could be very expensive.  This is default behavior so that
    // subclasses do not necessarily have to override.  Equality is eq.
    override def equals(other: Any): Boolean = {
      triEquals(other).getOrElse(false)
    }

    // This could be expensive, so therefore cache it.
    override def hashCode: Int = cachedHashCode

    protected lazy val cachedHashCode = calculateHashCode

    protected def calculateHashCode: Int = 0
  }

  class Subclass1(value: Int) extends Superclass(value) {

    override def equals(other: Any): Boolean = {
      super.triEquals(other).getOrElse {
        val that = other.asInstanceOf[Subclass1]

        this.value == that.value
      }
    }

    override protected def calculateHashCode: Int = value
  }

  class Subclass2(value: Int, var increment: Int) extends Superclass(value) {

    override def equals(other: Any): Boolean = {
      super.triEquals(other).getOrElse {
        val that = other.asInstanceOf[Subclass2]

        this.value == that.value &&
        this.increment == that.increment
      }
    }

    override protected def calculateHashCode: Int = value + increment
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

  it should "unequal for superclass copies" in {
    val one = new Superclass(5)
    val two = new Superclass(5)

    one == two should be (false)
  }
}
