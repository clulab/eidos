package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.{FileUtils, StringUtils}

object DiffDirs extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val inputExt = "json"
  val outputExt = "jsonld"

  val inputFiles = FileUtils.findFiles(inputDir, inputExt)
  val outputFiles = FileUtils.findFiles(inputDir, outputExt)

  val inputMap = inputFiles.map { file => (StringUtils.beforeLast(file.getName, '.', true)) -> file }.toMap
  val outputMap = outputFiles.map { file => (StringUtils.beforeLast(file.getName, '.', true)) -> file }.toMap

  assert(inputMap.size == inputFiles.length)
  assert(outputMap.size == outputFiles.length)

  val inputSet = inputMap.keySet
  val outputSet = outputMap.keySet

  val onlyInput = inputSet.diff(outputSet)
  val onlyOutput = outputSet.diff(inputSet)

  println("OnlyInput:")
  onlyInput.foreach(println)

  println()
  println("OnlyOutput:")
  onlyOutput.foreach(println)
}
