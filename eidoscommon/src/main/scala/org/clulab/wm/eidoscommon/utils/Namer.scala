package org.clulab.wm.eidoscommon.utils

trait Namer {
  def name: String // gets the entire name with parts separated by /
  def branch: Option[String] // gets the branch in top/branch/[more/]leaf

  def canonicalName: String = Namer.canonicalize(name)

  override def toString: String = name
}

object Namer {
  implicit val NameOrdering: Ordering[Namer] = new Ordering[Namer] {
    override def compare(x: Namer, y: Namer): Int = x.name.compare(y.name)
  }

  def canonicalize(name: String): String = {
    val longName = name
    val shortName = StringUtils.afterLast(longName, '/', true)
    // Use '' instead of "" to avoid regular expression replacement of strings.  We have simple characters here.
    val separatedName = shortName.replace('_', ' ')
    val lowerName = separatedName.toLowerCase

    lowerName
  }
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