package org.clulab.wm.wmexchanger.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils

import java.io.File

class MockKafkaConsumer(inputDir: String, outputDir: String) extends KafkaConsumerish {

  // This cheats by copying the answer from the inputDir and moving them
  // to the outputDir where something else is waiting for them.
  def this(outputDir: String) = this(
    FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath,
    outputDir
  )

  def poll(duration: Int): Unit = {
    Thread.sleep(duration)
    LockUtils.cleanupLocks(outputDir, Extensions.lock, Extensions.json)

    val files = FileUtils.findFiles(inputDir, Extensions.json)

    if (files.nonEmpty) {
      val inputFile = files.head
      val outputFile = FileEditor(inputFile).setDir(outputDir).get
      FileUtils.rename(inputFile, outputFile)
      val lockFile = FileEditor(outputFile).setExt(Extensions.lock).get
      lockFile.createNewFile()
    }
  }

  def close(): Unit = ()
}
