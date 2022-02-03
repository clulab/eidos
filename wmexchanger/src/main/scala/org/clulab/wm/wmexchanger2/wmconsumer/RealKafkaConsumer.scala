package org.clulab.wm.wmexchanger2.wmconsumer

import org.apache.kafka.clients.consumer.{KafkaConsumer => ApacheKafkaConsumer}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils, Logging}
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger2.utils.FileName
import org.json4s._

import java.io.File
import java.time.Duration
import java.util.{Collections, ConcurrentModificationException, Properties}

class RealKafkaConsumer(appProperties: Properties, kafkaProperties: Properties, distinguisher: Counter)
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
      val outputFile = FileName(key).setExt(Extensions.json).distinguish(0, distinguisher).setDir(outputDir).toFile

      logger.info("Consuming " + outputFile.getName)
      LockUtils.withLock(outputFile, Extensions.lock) {
        FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
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
