package org.clulab.wm.eidoscommon.utils

trait Namer {
  def name: String // gets the entire name with parts separated by /
  def branch: Option[String] // gets the branch in top/branch/[more/]leaf

  def canonicalName: String = Namer.canonicalize(name)
  def canonicalWords: Array[String] = Namer.canonicalizeWords(name)

  override def toString: String = name
}

object Namer {
  implicit val NameOrdering: Ordering[Namer] = new Ordering[Namer] {
    override def compare(x: Namer, y: Namer): Int = x.name.compare(y.name)
  }

  def canonicalize(name: String): String = {
    val shortName = StringUtils.afterLast(name, '/', true)
    val lowerName = shortName.toLowerCase
    val separatedName = lowerName.replace('_', ' ')

    separatedName
  }

  def canonicalizeWords(name: String): Array[String] = {
    val shortName = StringUtils.afterLast(name, '/', true)

    if (shortName.nonEmpty) {
      val lowerName = shortName.toLowerCase
      val separatedNames = lowerName.split('_')

      separatedNames
    }
    else
      Array.empty
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