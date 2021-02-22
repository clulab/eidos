package org.clulab.wm.eidos.apps.batch

import java.io.File

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidos.utils.meta.CdrText
import org.clulab.wm.eidos.utils.meta.CluText
import org.json4s.DefaultFormats
import org.json4s.JValue

object SeparateCdrTextFromDirectory extends App {

  class Filter(outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, cdrText: CdrText): Unit = {
      println(s"Extracting from ${inputFile.getName}")
      val text: String = cdrText.getText
      val outputFile = FileEditor(inputFile).setDir(outputDir).setExt("txt").get

      FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
        printWriter.print(text)
      }
    }
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val inputFiles = FileUtils.findFiles(inputDir, "json")
  val filter = new Filter(outputDir)

  inputFiles.foreach { inputFile =>
    filter.filter(inputFile, CdrText(inputFile))
  }
}
