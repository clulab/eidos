package org.clulab.wm.wmexchanger.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger.utils.WmUserApp

import java.util.Properties

class KafkaConsumerLoopApp(args: Array[String]) extends WmUserApp(args,  "/kafkaconsumer.properties") {
  var useReal = KafkaConsumerLoopApp.useReal

  val localKafkaProperties: Properties = {
    // This allows the login to be contained in a file external to the project.
    val loginProperty = appProperties.getProperty("login")
    val loginPropertiesBuilder = PropertiesBuilder.fromFile(loginProperty)

    PropertiesBuilder(kafkaProperties).putAll(loginPropertiesBuilder).get
  }

  val topic: String = appProperties.getProperty("topic")
  val outputDir: String = appProperties.getProperty("outputDir")
  FileUtils.ensureDirsExist(outputDir)

  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val closeDuration: Int = appProperties.getProperty("close.duration").toInt

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp.logger, interactive, waitDuration) {

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
      val consumer =
          if (useReal) new KafkaConsumer(localKafkaProperties, closeDuration, topic, outputDir, lock = true)
          else new MockKafkaConsumer(outputDir)
      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { consumer.close() }

      while (!isInterrupted) {
        consumer.poll(pollDuration)
      }
      consumer.close()
    }
  }
}

object KafkaConsumerLoopApp extends LoopApp {
  var useReal = DevtimeConfig.useReal

  def main(args: Array[String]): Unit = {
    args.foreach(println)
    loop {
      () => new KafkaConsumerLoopApp(args).thread
    }
  }
}
