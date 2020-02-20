package ai.lum.eidos.kafka.producer

import java.util.Properties

import org.apache.kafka.clients.producer.KafkaProducer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.clients.producer.RecordMetadata
import org.apache.kafka.common.serialization.Serdes

import scala.concurrent.ExecutionContext
import scala.concurrent.Future // this is built in in scala 2.13

class ExampleStream(topic: String, bootstrapServers: String)(implicit executionContext: ExecutionContext) {
  protected val producer: KafkaProducer[String, String] = {
    val kafkaProps: Properties = {
      val serializer = Serdes.String().serializer
      val props = new Properties()

      props.put("bootstrap.servers", bootstrapServers)
      props.put("key.serializer", serializer.getClass.getName)
      props.put("value.serializer", serializer.getClass.getName)
      props
    }

    new KafkaProducer[String, String](kafkaProps)
  }

  def send(key: String, value: String): Future[RecordMetadata] = {
    val message = new ProducerRecord[String, String](topic, key, value)

    Future(producer.send(message).get)
  }
}
