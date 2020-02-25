package ai.lum.eidos.kafka.apps

import java.time.Duration

import ai.lum.eidos.kafka.stream.EidosStream

object EidosApp extends App {
  val eidosStream = new EidosStream(
    applicationId = "eidos-stream",
    bootstrapServers = "localhost:9092",
    inputTopic = "eidos-input",
    outputTopic = "eidos-output"
  )
  val streams = eidosStream.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
