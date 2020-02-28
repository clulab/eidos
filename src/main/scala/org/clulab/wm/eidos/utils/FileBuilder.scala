package org.clulab.wm.eidos.utils

import java.io.File

class FileBuilder(protected var file: File) {
  import FileBuilder._

  def get = file

  def changeDir(dir: String): FileBuilder = {
    val name = file.getName
    val newPath = dir + slashs + name

    file = new File(newPath)
    this
  }

  def changeName(name: String): FileBuilder = {
    val pathOpt = Option(file.getParentFile).map(_.getPath + slashs).getOrElse("")
    val newPath = pathOpt + name

    file = new File(newPath)
    this
  }

  def appendName(name: String): FileBuilder = {
    file = new File(file.getPath + name)
    this
  }

  def changeExt(ext: String): FileBuilder = {
    // Being an extension implies that it starts with a period, but don't add a second period,
    // because then it's not an extension but something else.
    val dottedExt = if (ext.startsWith(dots)) ext else dots + ext
    val path = file.getPath
    val extensionlessPath = StringUtils.beforeLast(path, dot)
    val newPath = extensionlessPath + dottedExt

    file = new File(newPath)
    this
  }
}

object FileBuilder {
  val dot = '.'
  val dots = dot.toString
  val slash = '/'
  val slashs = slash.toString

  def apply(file: File): FileBuilder = new FileBuilder(file)
}