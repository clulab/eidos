package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.{DevtimeConfig, LoopApp, SafeThread}

import java.util.Properties

class KafkaConsumerLoopApp(args: Array[String]) {
  var useReal = KafkaConsumerLoopApp.useReal

  private val config : Config = ConfigFactory.defaultApplication().resolve()
  private val appProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.app")).get
  private val kafkaProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.consumer")).get

  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  // TODO: Move this below so that it only affects the Mock version.
  val outputDir: String = appProperties.getProperty("output.dir")

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp.logger, interactive, waitDuration) {

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
      val consumer =
          if (useReal)
            new KafkaConsumer(appProperties, kafkaProperties, lock = true)
          else {
            // In some tests with useReal = false, the outputDir is passed in args.
            val mockOutputDir = LoopApp
                .getNamedArg(args, "app.outputDir")
                .getOrElse(outputDir)
            new MockKafkaConsumer(mockOutputDir)
          }
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
    loop {
      () => new KafkaConsumerLoopApp(args).thread
    }
  }
}
