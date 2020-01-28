package org.clulab.wm.eidos.utils

trait Namer {
  def name: String // gets the entire name with parts separated by /
  def branch: Option[String] // gets the branch in top/branch/[more/]leaf

  override def toString: String = name
}

class PassThruNamer(val name: String) extends Namer {

  def branch: Option[String] = {
    val count = name.count( char => char == '/')

    if (count >= 2)
      Some(StringUtils.beforeFirst(StringUtils.afterFirst(name, '/', false), '/', false))
    else
      None
  }
}