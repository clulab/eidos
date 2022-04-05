package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils}
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumer.logger
import org.clulab.wm.wmexchanger2.utils.FileName

class MockKafkaConsumer(inputDir: String, outputDir: String, stage: Int, distinguisher: Counter) extends KafkaConsumerish {
  FileUtils.ensureDirsExist(outputDir)

  def poll(duration: Int): Unit = {
    Thread.sleep(duration)

    val files = FileUtils.findFiles(inputDir, Extensions.json)

    if (files.nonEmpty) {
      files.foreach { inputFile =>
        val outputFile = FileName(inputFile).distinguish(stage, distinguisher).setDir(outputDir).toFile

        Thread.sleep(100)
        logger.info("Consuming " + outputFile.getName)
        LockUtils.withLock(outputFile, Extensions.lock) {
          FileUtils.rename(inputFile, outputFile)
        }
      }
    }
  }

  def close(): Unit = ()
}
