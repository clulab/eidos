package org.clulab.wm.wmexchanger.wmconsumer

import java.util.Properties

import org.clulab.wm.wmexchanger.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.WmUserApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class KafkaConsumerApp(args: Array[String]) extends WmUserApp(args,  "/kafkaconsumer.properties") {
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

  val thread: SafeThread = new SafeThread(KafkaConsumerApp.logger) {
    override def runSafely(): Unit = {
      val consumer = new KafkaConsumer(localKafkaProperties, closeDuration, topic, outputDir)

      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { consumer.close() }
      while (!isInterrupted)
        consumer.poll(pollDuration)
    }
  }

  if (interactive)
    thread.waitSafely(waitDuration)
}

object KafkaConsumerApp extends App {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  new KafkaConsumerApp(args)
}
