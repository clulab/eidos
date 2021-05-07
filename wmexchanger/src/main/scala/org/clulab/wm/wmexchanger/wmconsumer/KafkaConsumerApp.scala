package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.{Logging, PropertiesBuilder}
import org.clulab.wm.wmexchanger.utils.SafeThread

import java.util.Properties

class KafkaConsumerApp(args: Array[String]) {
  private val config : Config = ConfigFactory.defaultApplication().resolve()
  private val appProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.app")).get
  private val kafkaProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.consumer")).get

  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt

  val thread: SafeThread = new SafeThread(KafkaConsumerApp.logger) {

    override def runSafely(): Unit = {
      val consumer = new KafkaConsumer(appProperties, kafkaProperties)

      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { consumer.close() }
      while (!isInterrupted)
        consumer.poll(pollDuration)
    }
  }

  if (interactive)
    thread.waitSafely(waitDuration)
}

object KafkaConsumerApp extends App with Logging {
  new KafkaConsumerApp(args)
}
