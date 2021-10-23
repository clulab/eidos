package org.clulab.wm.eidoscommon.utils

trait Namer {
  def getName: String // gets the entire name with parts separated by /
  def getBranch: Option[String] // gets the branch in top/branch/[more/]leaf
  def getSimpleName: String // gets the leaf name

  override def toString: String = getName
}

object Namer {

  def getBranch(name: String): Option[String] = {
    val count = name.count( char => char == '/')

    if (count >= 2)
      Some(StringUtils.beforeFirst(StringUtils.afterFirst(name, '/', false), '/', false))
    else
      None
  }

  def getSimpleName(name: String): String = StringUtils.afterLast(name, '/', all = true)
}

// This is mostly for deserialization.  When we read back a serialization,
// it may not be possible to match it back up to something with nodes.
class PassThruNamer(val name: String) extends Namer {

  def getBranch: Option[String] = Namer.getBranch(name)

  def getName: String = name

  def getSimpleName: String = Namer.getSimpleName(name)
}
