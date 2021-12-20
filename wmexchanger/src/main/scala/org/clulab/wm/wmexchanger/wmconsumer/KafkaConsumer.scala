package org.clulab.wm.wmexchanger.wmconsumer

import org.apache.kafka.clients.consumer.{KafkaConsumer => ApacheKafkaConsumer}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils, Logging}
import org.clulab.wm.wmexchanger.utils.Extensions
import org.json4s._

import java.io.File
import java.time.Duration
import java.util.{Collections, ConcurrentModificationException, Properties}

class KafkaConsumer(appProperties: Properties, kafkaProperties: Properties)
    extends KafkaConsumerish {
  import org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumer._
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  logger.info("Opening consumer...")
  val topic: String = appProperties.getProperty("topic")
  val outputDir: String = appProperties.getProperty("output.dir")
  FileUtils.ensureDirsExist(outputDir)
  val closeDuration: Int = appProperties.getProperty("close.duration").toInt

  protected val consumer: ApacheKafkaConsumer[String, String] = {
    val consumer = new ApacheKafkaConsumer[String, String](kafkaProperties)

    consumer.subscribe(Collections.singletonList(topic))
    consumer
  }

  def poll(duration: Int): Unit = {
    val records = consumer.poll(Duration.ofSeconds(duration))

    logger.info(s"Polling ${records.count} records...")
    records.forEach { record =>
      val key = record.key
      val value = record.value
      val file = FileEditor(new File(key + ".")).setDir(outputDir).setExt(Extensions.json).get
      logger.info("Consuming " + file.getName)

      LockUtils.withLock(file, Extensions.lock) {
        FileUtils.printWriterFromFile(file).autoClose { printWriter =>
          printWriter.print(value)
        }
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

object KafkaConsumer extends Logging
