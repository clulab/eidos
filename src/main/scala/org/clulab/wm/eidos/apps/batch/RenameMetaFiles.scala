package org.clulab.wm.eidos.apps.batch

import java.nio.file.Files
import java.nio.file.Paths

import org.clulab.wm.eidos.utils.FileUtils

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

  textFiles.foreach { textFile =>
    val textFileName = textFile.getName()
    val inMetaFileName = beforeLast(afterLast(textFileName, '_'), '.') + ".json"
    val outMetaFileName = beforeLast(textFileName, '.', true) + ".json"
    val metaFile = metaFiles.find(metaFile => metaFile.getName() == inMetaFileName)

    if (metaFile.isDefined) {
      try {
        val fromPath = metaFile.get.toPath()
        val toPath = Paths.get(outputDir + "/" + outMetaFileName)

        println("Renaming " + fromPath + " to " + toPath)
        Files.move(fromPath, toPath)
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
