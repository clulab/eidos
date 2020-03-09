package ai.lum.eidos.kafka.producer

import java.io.File
import java.time.Duration
import java.util.Properties

import org.apache.kafka.clients.producer.KafkaProducer
import org.apache.kafka.clients.producer.ProducerRecord

import org.clulab.wm.eidos.utils.FileUtils

class EidosProducer(topic: String, properties: Properties) {
  protected val producer = new KafkaProducer[String, String](properties)

  def send(file: File): Unit = {
    val key = file.getName
    val value = FileUtils.getTextFromFile(file)
    val message = new ProducerRecord(topic, key, value)

    println("Produce " + file.getName)
    producer.send(message)
  }

  def close(): Unit = {
    producer.flush()
    println("Closing producer")
    producer.close(Duration.ofSeconds(10))
  }
}
