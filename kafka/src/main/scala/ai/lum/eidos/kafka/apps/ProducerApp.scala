package ai.lum.eidos.kafka.apps

import java.util.Properties

import ai.lum.eidos.kafka.producer.ExampleProducer
import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success}

object ProducerApp {
  protected val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
  protected val TARGET_TOPIC = "target.topic"
  protected val KAFKA_URL = "kafka.url"
  protected val LOG: Logger = LoggerFactory.getLogger(getClass)
  protected val DEFAULT_PROPS: Properties = PropertiesBuilder()
      .put(TARGET_TOPIC, "eidos.test")
      .put(KAFKA_URL, "kafka-broker-1:19092") // Do these names have symbolic constants?
      .get
  protected val producer: ExampleProducer = new ExampleProducer(
    DEFAULT_PROPS.getProperty(TARGET_TOPIC),
    DEFAULT_PROPS.getProperty(KAFKA_URL)
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
