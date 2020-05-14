package org.clulab.wm.wmconsumer

import java.io.File
import java.time.Duration
import java.util.Collections
import java.util.ConcurrentModificationException
import java.util.Properties

import org.apache.kafka.clients.consumer.{KafkaConsumer => ApacheKafkaConsumer}
import org.clulab.wm.utils.Closer.AutoCloser
import org.clulab.wm.utils.FileUtils
import org.clulab.wm.utils.FileEditor
import org.json4s._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class KafkaConsumer(properties: Properties, closeDuration: Int, topic: String, outputDir: String) {
  import KafkaConsumer._
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  logger.info("Opening consumer...")

  protected val consumer: ApacheKafkaConsumer[String, String] = {
    val consumer = new ApacheKafkaConsumer[String, String](properties)

    consumer.subscribe(Collections.singletonList(topic))
    consumer
  }

  def poll(duration: Int): Unit = {
    val records = consumer.poll(Duration.ofSeconds(duration))

    logger.info(s"Polling ${records.count} records...")
    records.forEach { record =>
      val key = record.key
      val value = record.value
      // Imply an extension on the file so that it can be replaced.
      val file = FileEditor(new File(key + ".")).setDir(outputDir).setExt("json").get
      logger.info("Consuming " + file.getName)

      FileUtils.printWriterFromFile(file).autoClose { printWriter =>
        printWriter.print(value)
      }
    }
  }

  def close(): Unit = {
    logger.info("Closing consumer...")
    try {
      consumer.close(Duration.ofSeconds(closeDuration))
    }
    catch {
      case _: ConcurrentModificationException => // KafkaConsumer is not safe for multi-threaded access
    }
  }
}

object KafkaConsumer {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
