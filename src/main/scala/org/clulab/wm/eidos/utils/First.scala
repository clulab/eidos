package org.clulab.wm.eidos.utils

class First {
  protected var first = true

  protected def getAndSet(): Boolean = {
    val oldFirst = first

    first = false
    oldFirst
  }

  def isTrue: Boolean = getAndSet()

  def isFalse: Boolean = !getAndSet()
}

object First {

  def apply() = new First
}
