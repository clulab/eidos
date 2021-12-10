package org.clulab.wm.eidoscommon.utils

class IntHolder(val value: Int) {

  override def equals(other: Any): Boolean =
    this.value == other.asInstanceOf[IntHolder].value

  override def hashCode: Int = value
}
