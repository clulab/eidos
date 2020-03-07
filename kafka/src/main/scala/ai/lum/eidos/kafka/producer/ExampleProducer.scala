package ai.lum.eidos.kafka.producer

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.apache.kafka.clients.producer.KafkaProducer
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.clients.producer.RecordMetadata
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ExampleProducer(topic: String, bootstrapServers: String)(executionContext: ExecutionContext) {
  protected val producer: KafkaProducer[String, String] = {
    val properties = PropertiesBuilder()
        .put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
        .put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
        .put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
        .get

    new KafkaProducer[String, String](properties)
  }

  def send(key: String, value: String): Future[RecordMetadata] = {
    val message = new ProducerRecord[String, String](topic, key, value)

    Future {
      println(s"Message $key is going to sleep...")
      Thread.sleep(2000)
      println(s"Message $key is waking up...")
      val result = producer.send(message).get
      println(s"Message $key result is ready...")
      result
    }(executionContext)
  }
}
