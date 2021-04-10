package org.clulab.wm.wmexchanger.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger.utils.WmUserApp

import java.io.File
import java.util.Properties

class MockKafkaConsumer(inputDir: String, outputDir: String) {

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
      if (outputFile.exists)
        outputFile.delete()
      inputFile.renameTo(outputFile)
      val lockFile = FileEditor(outputFile).setExt(Extensions.lock).get
      lockFile.createNewFile()
    }
  }

  def close(): Unit = ()
}

class KafkaConsumerLoopApp(args: Array[String]) extends WmUserApp(args,  "/kafkaconsumer.properties") {
  val localKafkaProperties: Properties = {
    // This allows the login to be contained in a file external to the project.
    val loginProperty = appProperties.getProperty("login")
    val loginPropertiesBuilder = PropertiesBuilder.fromFile(loginProperty)

    PropertiesBuilder(kafkaProperties).putAll(loginPropertiesBuilder).get
  }

  val topic: String = appProperties.getProperty("topic")
  val outputDir: String = appProperties.getProperty("outputDir")

  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val closeDuration: Int = appProperties.getProperty("close.duration").toInt

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp.logger, interactive, waitDuration) {

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
//      val consumer = new KafkaConsumer(localKafkaProperties, closeDuration, topic, outputDir, lock = true)
      val consumer = new MockKafkaConsumer(outputDir)
      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { consumer.close() }

      while (!isInterrupted) {
        consumer.poll(pollDuration)
      }
      consumer.close()
    }
  }
}

object KafkaConsumerLoopApp extends App with LoopApp {
  args.foreach(println)
  loop {
    () => new KafkaConsumerLoopApp(args).thread
  }
}
