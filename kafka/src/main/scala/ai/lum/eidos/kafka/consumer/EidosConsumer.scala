package ai.lum.eidos.kafka.consumer

import java.io.File
import java.time.Duration
import java.util.Collections
import java.util.Properties

import org.apache.kafka.clients.consumer.KafkaConsumer

import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

class EidosConsumer(topic: String, properties: Properties, outputDir: String) {
  protected val consumer = {
    val consumer = new KafkaConsumer[String, String](properties)

    consumer.subscribe(Collections.singletonList(topic))
    consumer
  }

  def poll(): Unit = {
    val records = consumer.poll(Duration.ofSeconds(10))

    records.forEach { record =>
      val file = FileEditor(new File(record.key)).setDir(outputDir).setExt("jsonld").get
      val value = record.value

      println("Consume " + file.getName)
      FileUtils.printWriterFromFile(file).autoClose { printWriter =>
        printWriter.print(value)
      }
    }
  }

  def close(): Unit = {
    println("Closing consumer")
    consumer.close(Duration.ofSeconds(10))
  }
}
