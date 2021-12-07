package org.clulab.wm.eidoscommon.utils

trait Namer {
  def getName: String // gets the entire name with parts separated by /
  def getBranchOpt: Option[String] // gets the branch in top/branch/[more/]leaf
  def getSimpleName: String // gets the leaf name

  def canonicalName: String = Namer.canonicalize(getName)
  def canonicalWords: Array[String] = Namer.canonicalizeWords(getName)

  override def toString: String = getName
}

object Namer {
  implicit val NameOrdering: Ordering[Namer] = new Ordering[Namer] {
    override def compare(x: Namer, y: Namer): Int = x.getName.compare(y.getName)
  }

  def getBranch(name: String): Option[String] = {
    val count = name.count( char => char == '/')

    if (count >= 2)
      Some(StringUtils.beforeFirst(StringUtils.afterFirst(name, '/', false), '/', false))
    else
      None
  }

  def getSimpleName(name: String): String = StringUtils.afterLast(name, '/', all = true)

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

// This is mostly for deserialization.  When we read back a serialization,
// it may not be possible to match it back up to something with nodes.
class PassThruNamer(val name: String) extends Namer {

  def getBranchOpt: Option[String] = Namer.getBranch(name)

  def getName: String = name

  def getSimpleName: String = Namer.getSimpleName(name)
}
