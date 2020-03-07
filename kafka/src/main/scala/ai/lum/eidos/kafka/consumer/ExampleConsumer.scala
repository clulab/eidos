package ai.lum.eidos.kafka.consumer

import ai.lum.eidos.kafka.utils.PropertiesBuilder

import java.time.Duration
import java.util.Collections

import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.ExecutionContext

class ExampleConsumer(topic: String, bootstrapServers: String)(executionContext: ExecutionContext) {
  protected val consumer: KafkaConsumer[String, String] = {
    val properties = PropertiesBuilder()
        .put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
        .put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
        .put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
        .get
    val consumer = new KafkaConsumer[String, String](properties)

    consumer.subscribe(Collections.singletonList(topic))
    consumer
  }

  def poll(duration: Duration): Unit = {
    val consumerRecords = consumer.poll(duration)

    consumerRecords.forEach { record =>
      System.out.println(s"${record.key}: ${record.value}")
    }
    consumer.commitAsync()
  }

  def close(): Unit = consumer.close()
}
