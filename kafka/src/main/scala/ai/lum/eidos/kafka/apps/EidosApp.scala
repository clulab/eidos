package ai.lum.eidos.kafka.apps

import java.time.Duration

import ai.lum.eidos.kafka.stream.EidosStream
import ai.lum.eidos.kafka.utils.Counter

object EidosApp extends App {
  val threads = args(0).toInt
  val counter = Counter()

  val exampleStream = new EidosStream(
    counter,
    applicationId = "eidos-stream",
    bootstrapServers = "localhost:9092",
    inputTopic = "eidos-input",
    outputTopic = "eidos-output",
    threads
  )
  val streams = exampleStream.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
