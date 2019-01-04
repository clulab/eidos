package org.clulab.wm.eidos.utils

trait QuicklyEqualable {

  // Default behavior is to require an exact match on the classes.
  protected def canEqual(other: AnyRef): Boolean = this.getClass == other.getClass

  // Tri because this is tri-state logic: yes, no, maybe.
  protected def triEquals(other: Any): Option[Boolean] = {
    // The order of these first few tests will have a slight influence on efficiency.
    if (!other.isInstanceOf[AnyRef])
      Some(false) // Don't allow this AnyRef to equal an AnyVal.
    else {
      val anyOther = other.asInstanceOf[AnyRef]

      if (!this.canEqual(anyOther))
        Some(false)
      else if (this.eq(anyOther))
        Some(true) // Shortcut comparisons to self.
      else if (this.hashCode != other.hashCode)
        Some(false)
      else None
    } // Leave it up to equals.
    // Is it necessary to do that.canEqual(this)?
    // Not if they are both the exact same class.
  }

  /**
    * This is what subclasses should implement.  They should
    * normally check with the superclass first, so super.biEquals(other).
    * The top class should not consult this one, however, at least
    * not expecting true out of it.  false is here so that the
    * result is only true if triEquals has decided in equals below.
    * In the end one gets this.eq(other) by default.
    *
    * Note that biEquals should be called by equals after triEquals.
    * triEquals will have already checked with canEqual that other
    * has the same class as this.  In biEquals it is therefore possible
    * to blindly cast to the class of this, which should be faster than
    * matching.  Even though that's not the Scala way, for the sake of
    * efficiency of QuicklyEqualable, that's how it's just now implemented.
    *
    * For example,
    *
    * override def biEquals(other: Any): Boolean = {
    *   val that = other.asInstanceOf[ContextAttachmentSpec]
    *
    *   this.text == that.text
    * }
    *
    * is being favored over
    *
    * override def biEquals(other: Any): Boolean = other match {
    *   case that: ContextAttachmentSpec => this.text == that.text
    * }
    */
  protected def biEquals(other: Any): Boolean = false

  // This could be very expensive.  This is default behavior so that
  // subclasses do not necessarily have to override.  Equality is eq
  // which will be caught in triEquals, otherwise they are unequal.
  override def equals(other: Any): Boolean = {
    triEquals(other).getOrElse(biEquals(other))
  }

  // This could be expensive, so therefore cache it.
  override def hashCode: Int = cachedHashCode

  protected lazy val cachedHashCode: Int = calculateHashCode

  // Default implementation that gives everything the same hash.
  protected def calculateHashCode: Int = 0
}
