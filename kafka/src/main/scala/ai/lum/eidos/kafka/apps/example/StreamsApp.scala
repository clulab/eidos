package ai.lum.eidos.kafka.apps.example

import java.time.Duration
import java.util.Properties

import ai.lum.eidos.kafka.stream.ExampleStream
import ai.lum.eidos.kafka.utils.PropertiesBuilder

import org.apache.kafka.streams.StreamsConfig
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object StreamsApp extends App {
  protected val LOG: Logger = LoggerFactory.getLogger(getClass)

  protected val DEFAULT_PROPS: Properties = PropertiesBuilder()
      .put(StreamsConfig.APPLICATION_ID_CONFIG, "example-stream")
      .put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
      .get

  val exampleStream = new ExampleStream(
    applicationId = DEFAULT_PROPS.getProperty(StreamsConfig.APPLICATION_ID_CONFIG),
    bootstrapServers = DEFAULT_PROPS.getProperty(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG),
    inputTopic = ProducerApp.DEFAULT_PROPS.getProperty(ProducerApp.TOPIC),
    outputTopic = ConsumerApp.DEFAULT_PROPS.getProperty(ConsumerApp.TOPIC)
  )
  val streams = exampleStream.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
