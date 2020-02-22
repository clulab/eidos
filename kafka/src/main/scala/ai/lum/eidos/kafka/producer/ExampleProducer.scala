package ai.lum.eidos.kafka.producer

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.serialization.Serdes

import scala.concurrent.ExecutionContext
import scala.concurrent.Future // this is built in in scala 2.13

class ExampleProducer(topic: String, bootstrapServers: String)(executionContext: ExecutionContext) {
  protected val producer: KafkaProducer[String, String] = {
    val properties = {
      val serializer = Serdes.String().serializer

      PropertiesBuilder()
          .put("bootstrap.servers", bootstrapServers)
          .put("key.serializer", serializer.getClass.getName)
          .put("value.serializer", serializer.getClass.getName)
          .get
    }

    new KafkaProducer[String, String](properties)
  }

  def send(key: String, value: String): Future[RecordMetadata] = {
    val message = new ProducerRecord[String, String](topic, key, value)

    // Why does it need to be this complicated?
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
