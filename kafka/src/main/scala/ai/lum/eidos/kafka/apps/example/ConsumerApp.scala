package ai.lum.eidos.kafka.apps.example

import ai.lum.eidos.kafka.consumer.ExampleConsumer
import ai.lum.eidos.kafka.utils.PropertiesBuilder
import java.time.Duration
import java.util.Properties

import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerConfig
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

object ConsumerApp {
  protected val LOG: Logger = LoggerFactory.getLogger(getClass)
  protected val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  val TOPIC = "consumer.topic"

  val DEFAULT_PROPS: Properties = PropertiesBuilder()
      .put(TOPIC, "example-output")
      .put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
      .put(ConsumerConfig.GROUP_ID_CONFIG, "example-consumer-group")
      .put(ConsumerConfig.CLIENT_ID_CONFIG, "example-consumer")
      .get
  protected val consumer: ExampleConsumer = new ExampleConsumer(
    DEFAULT_PROPS.getProperty(TOPIC),
    DEFAULT_PROPS.getProperty(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG),

    DEFAULT_PROPS.getProperty(ConsumerConfig.GROUP_ID_CONFIG)
  )(executionContext)

  def main(args: Array[String]): Unit = {
    var count = 0

    while (count < 100) {
      consumer.poll(Duration.ofSeconds(10))
      count += 1
    }

    consumer.close()
  }
}
