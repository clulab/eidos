package ai.lum.eidos.kafka.apps

import java.time.Duration

import ai.lum.eidos.kafka.stream.ExampleStream

object StreamsApp extends App {
  val exampleStream = new ExampleStream(
    applicationId = "eidos-stream",
    bootstrapServers = "localhost:9092",
    inputTopic = "eidos-input",
    outputTopic = "eidos-output"
  )
  val streams = exampleStream.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
