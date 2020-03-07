package ai.lum.eidos.kafka.apps.example

import java.util.Properties

import ai.lum.eidos.kafka.producer.ExampleProducer
import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.apache.kafka.clients.producer.ProducerConfig
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Random
import scala.util.Success

object ProducerApp {
  protected val LOG: Logger = LoggerFactory.getLogger(getClass)
  protected val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  val TOPIC = "producer.topic"

  val DEFAULT_PROPS: Properties = PropertiesBuilder()
      .put(TOPIC, "example-input")
      .put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
      .get
  protected val producer: ExampleProducer = new ExampleProducer(
    DEFAULT_PROPS.getProperty(TOPIC),
    DEFAULT_PROPS.getProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG)
  )(executionContext)

  def main(args: Array[String]): Unit = {
    val random = new Random()
    var key: Int = 0

    while (true) {
      Thread.sleep(5000)

      producer.send(key.toString, random.nextString(10)).onComplete {
        case Success(value) => LOG.info(s"message sent successfully: $value")
        case Failure(exception) => exception.printStackTrace()
      }(executionContext)
      key += 1
    }
  }
}
