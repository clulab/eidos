package ai.lum.eidos.kafka.apps

import java.time.Duration

import ai.lum.eidos.kafka.stream.EidosStream
import ai.lum.eidos.kafka.utils.EidosSystem

object EidosApp extends App {
  val threads = args(0).toInt
  val eidosSystem = new EidosSystem()

  val eidosStream = new EidosStream(
    eidosSystem,
    applicationId = "eidos-stream",
    bootstrapServers = "localhost:9092",
    inputTopic = "eidos-input",
    outputTopic = "eidos-output",
    threads // This can probably be done in a configuration file
  )
  val streams = eidosStream.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
