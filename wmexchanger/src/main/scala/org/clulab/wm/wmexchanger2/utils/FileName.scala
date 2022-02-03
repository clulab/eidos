package org.clulab.wm.wmexchanger2.utils

import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File
import scala.util.Try

case class FileName(directories: String, names: IndexedSeq[String], distinguishers: IndexedSeq[String], extensions: IndexedSeq[String]) {
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

  def toFile: File = new File(toString)

  def setDir(dir: String): FileName = this.copy(directories = dir)

  def setExt(ext: String): FileName = this.copy(extensions = IndexedSeq(ext))

  def addExt(ext: String): FileName = this.copy(extensions = extensions :+ ext)

  def distinguish(n: Int, distinguisher: Counter): FileName = distinguish(n, distinguisher.getAndInc)

  def distinguish(n: Int, distinguisher: Int): FileName = distinguish(n, distinguisher.toString)

  def distinguish(n: Int, distinguisher: String): FileName = {
    if (n < distinguishers.length)
      this.copy(distinguishers = distinguishers.updated(n, distinguisher))
    else if (n == distinguishers.length)
      this.copy(distinguishers = distinguishers :+ distinguisher)
    else // n > distinguishers.length
      this.copy(distinguishers = distinguishers.padTo(n, "") :+ distinguisher)
  }

  def getName(n: Int): String = names(n)

  // TODO, what if n is too high?
  // Need a pad
  def setName(n: Int, name: String): FileName = this.copy(names = names :+ name)

  def getDocumentId: String = getName(0)

  def getOntologyId: String = getName(1)
}

object FileName {
  // directory/directory/directory/name_name_name+distinguisher+distinguisher.ext.ext
  // directory does not include /
  // name, distinguisher, and ext do not include /, _, -, or .
  // distinguishers are non-negative integers
  val directorySep = '/'
  val nameSep = '_'
  val distinguisherSep = '+'
  val extensionSep = '.'
  val empty = ""

  def splitBefore(string: String, char: Char): (String, String) =
      (StringUtils.beforeFirst(string, char), StringUtils.afterFirst(string, char))

  def splitAfter(string: String, char: Char): (String, String) =
      (StringUtils.afterLast(string, char), StringUtils.afterLast(string, char))

  def apply(file: File): FileName = apply(file.getPath)

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

  def getDistinguisher(n: Int, files: Seq[File]): Counter = {
    val fileNames = files.map { file => FileName(file.getPath) }
    val distinguishers = fileNames.flatMap { file =>
      file.distinguishers.lift(0).flatMap { distinguisher =>
        Try(distinguisher.toInt).toOption
      }
    }
    val maxDistinguisher =
        if (distinguishers.nonEmpty) distinguishers.max
        else -1

    Counter(maxDistinguisher + 1)
  }
}
