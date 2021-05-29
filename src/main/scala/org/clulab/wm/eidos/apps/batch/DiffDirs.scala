package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.{FileUtils, StringUtils}

object DiffDirs extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val inputExt = "json"
  val outputExt = "jsonld"

  val inputFiles = FileUtils.findFiles(inputDir, inputExt)
  val outputFiles = FileUtils.findFiles(outputDir, outputExt)

  val inputSet = inputFiles.map { file => (StringUtils.beforeLast(file.getName, '.', true)) }.toSet
  val outputSet = outputFiles.map { file => (StringUtils.beforeLast(file.getName, '.', true)) }.toSet

  assert(inputSet.size == inputFiles.length)
  assert(outputSet.size == outputFiles.length)

  val onlyInput = inputSet.diff(outputSet).toSeq.sorted
  val onlyOutput = outputSet.diff(inputSet).toSeq.sorted

  println("OnlyInput:")
  onlyInput.foreach(println)

  println()
  println("OnlyOutput:")
  onlyOutput.foreach(println)
}
