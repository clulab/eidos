package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils}
import org.clulab.wm.wmexchanger.utils.Extensions

import java.io.File

class MockKafkaConsumer(inputDir: String, outputDir: String, var distinguisher: Int) extends KafkaConsumerish {

  // This cheats by copying the answer from the inputDir and moving them
  // to the outputDir where something else is waiting for them.
  def this(outputDir: String, distinguisher: Int) = this(
    FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath,
    outputDir, distinguisher
  )

  def poll(duration: Int): Unit = {
    Thread.sleep(duration)

    val files = FileUtils.findFiles(inputDir, Extensions.json)

    if (files.nonEmpty) {
      val inputFile = files.head
      val outputFile = FileEditor(inputFile).distinguish(distinguisher).setDir(outputDir).get

      distinguisher += 1
      LockUtils.withLock(outputFile, Extensions.lock) {
        FileUtils.rename(inputFile, outputFile)
      }
    }
  }

  def close(): Unit = ()
}
