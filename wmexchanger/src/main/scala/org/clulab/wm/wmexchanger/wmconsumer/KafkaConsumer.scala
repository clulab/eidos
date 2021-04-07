package org.clulab.wm.wmexchanger.wmconsumer

import java.io.File
import java.time.Duration
import java.util.Collections
import java.util.ConcurrentModificationException
import java.util.Properties
import org.apache.kafka.clients.consumer.{KafkaConsumer => ApacheKafkaConsumer}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.json4s._

class KafkaConsumer(properties: Properties, closeDuration: Int, topic: String, outputDir: String, lock: Boolean = false) {
  import KafkaConsumer._
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  logger.info("Opening consumer...")

  protected val consumer: ApacheKafkaConsumer[String, String] = {
    val consumer = new ApacheKafkaConsumer[String, String](properties)

    consumer.subscribe(Collections.singletonList(topic))
    consumer
  }

  def poll(duration: Int): Unit = {
    if (lock)
      LockUtils.cleanupLocks(outputDir, Extensions.lock, Extensions.json)

    val records = consumer.poll(Duration.ofSeconds(duration))

    logger.info(s"Polling ${records.count} records...")
    records.forEach { record =>
      val key = record.key
      val value = record.value
      val file = FileEditor(new File(key + ".")).setDir(outputDir).setExt(Extensions.json).get
      logger.info("Consuming " + file.getName)

      FileUtils.printWriterFromFile(file).autoClose { printWriter =>
        printWriter.print(value)
      }
      // Now that the file is complete and closed, add a corresponding lock file.
      if (lock) {
        val lockFile = FileEditor(file).setExt(Extensions.lock).get
        lockFile.createNewFile()
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
