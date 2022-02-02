package org.clulab.wm.wmexchanger2.utils

import org.clulab.wm.eidoscommon.utils.StringUtils

class FileName(directories: String, names: Seq[String], distinguishers: Seq[String], extensions: Seq[String]) {
  // If a separator is not needed, it is not included in the string, which might make a round trip string comparison fail.

  override def toString: String = new StringBuffer()
      .append(directories)
      .append(if (directories.nonEmpty) FileName.directorySep else FileName.empty)
      .append(names.mkString(FileName.nameSep.toString))
      .append(if (distinguishers.nonEmpty) FileName.distinguisherSep else FileName.empty)
      .append(distinguishers.mkString(FileName.distinguisherSep.toString))
      .append(if (extensions.nonEmpty) FileName.extensionSep else FileName.empty)
      .append(extensions.mkString(FileName.extensionSep.toString))
      .toString
}

object FileName {
  // directory/directory/directory/name_name_name-distinguisher-distinguisher.ext.ext
  // directory does not include /
  // name, distinguisher, and ext do not include /, _, -, or .
  // distinguishers are non-negative integers
  val directorySep = '/'
  val nameSep = '_'
  val distinguisherSep = '-'
  val extensionSep = '.'
  val empty = ""

  def splitBefore(string: String, char: Char): (String, String) =
      (StringUtils.beforeFirst(string, char), StringUtils.afterFirst(string, char))

  def splitAfter(string: String, char: Char): (String, String) =
      (StringUtils.afterLast(string, char), StringUtils.afterLast(string, char))

  def apply(string: String): FileName = {
    assert(string.nonEmpty)

    val (directories, afterDirectories) =
        if (string.contains(directorySep)) splitAfter(string, directorySep)
        else (empty, string)
    val (names, afterNames) = {
      val (multiName, afterNames) =
          if (afterDirectories.contains(distinguisherSep)) splitBefore(afterDirectories, distinguisherSep)
          else if (afterDirectories.contains(extensionSep)) splitBefore(afterDirectories, extensionSep)
          else (afterDirectories, empty)

      (multiName.split(nameSep), afterNames)
    }
    val (distinguishers, afterDistinguishers) = {
      val (multiDistinguisher, afterDistinguishers) =
          if (afterNames.contains(extensionSep)) splitBefore(afterNames, extensionSep)
          else (afterNames, empty)

      (multiDistinguisher.split(distinguisherSep), afterDistinguishers)
    }
    val extensions = afterDistinguishers.split(extensionSep)

    new FileName(directories, names, distinguishers, extensions)
  }
}
