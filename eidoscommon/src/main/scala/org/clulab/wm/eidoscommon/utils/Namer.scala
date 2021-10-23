package org.clulab.wm.eidoscommon.utils

trait Namer {
  def getName: String // gets the entire name with parts separated by /
  def getBranch: Option[String] // gets the branch in top/branch/[more/]leaf
  def getSimpleName: String // gets the leaf name

  override def toString: String = getName
}

class PassThruNamer(val name: String) extends Namer {

  def getBranch: Option[String] = {
    val count = name.count( char => char == '/')

    if (count >= 2)
      Some(StringUtils.beforeFirst(StringUtils.afterFirst(name, '/', false), '/', false))
    else
      None
  }

  def getName: String = name

  def getSimpleName: String = StringUtils.afterLast(name, '/', all = true)
}
