package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File
import java.nio.file.Files

object ShortenFilenames extends App {
  val inputDir = args.lift(0).getOrElse(".")
  val outputDir = args.lift(1).getOrElse(inputDir + "/short")
  val extension = ".jsonld"
  val inputFiles = FileUtils.findFiles(inputDir, extension)

  FileUtils.ensureDirsExist(inputDir, outputDir)
  inputFiles.foreach { inputFile =>
    val outputFilename = outputDir + "/" + StringUtils.beforeFirst(inputFile.getName, '_') + extension

    Files.copy(inputFile.toPath, new File(outputFilename).toPath)
  }
}
