package org.clulab.wm.eidos.apps.batch

import java.nio.file.Files

import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils

object RenameMetaFiles extends App {
  val textDir = args(0)
  val inputDir = args(1)
  val outputDir = args(2)

  def afterLast(string: String, char: Char, all: Boolean = true): String = {
    val index = string.lastIndexOf(char)

    if (index < 0)
      if (all) string
      else ""
    else string.substring(index + 1)
  }

  def beforeFirst(string: String, char: Char, all: Boolean = true): String = {
    val index = string.indexOf(char)

    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index)
  }

  def beforeLast(string: String, char: Char, all: Boolean = true): String = {
    val index = string.lastIndexOf(char)

    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index)
  }

  val textFiles = FileUtils.findFiles(textDir, "txt")
  val metaFiles = FileUtils.findFiles(inputDir, "json")

  // Text files must have something_somethingElse.txt
  // inMetaFileName is somethingElse.json.  If there is something like this, copy from it
  // outMetaFile is something_somethingElse.json.  to this in the output dir.
  textFiles.foreach { textFile =>
    val inMetaFile = FileEditor(textFile).setName(StringUtils.afterLast(textFile.getName, '_')).setExt("json").get
    val outMetaFile = FileEditor(textFile).setExt("json").get
    val metaFile = metaFiles.find(metaFile => metaFile.getName == inMetaFile.getName)

    if (metaFile.isDefined) {
      try {
        val fromFile = metaFile.get
        val toFile = FileEditor(outMetaFile).setDir(outputDir).get

        println("Moving " + fromFile + " to " + toFile)
        Files.move(fromFile.toPath, toFile.toPath)
      }
      catch {
        case exception: Exception =>
          println(s"Exception for file $metaFile")
          exception.printStackTrace()
      }
    }
    else {
      // remove metafile?
    }
  }
}
