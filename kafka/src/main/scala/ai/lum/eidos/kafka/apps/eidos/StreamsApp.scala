package ai.lum.eidos.kafka.apps.eidos

import ai.lum.eidos.kafka.stream.EidosStream
import ai.lum.eidos.kafka.utils.Counter
import ai.lum.eidos.kafka.utils.EidosSystem
import ai.lum.eidos.kafka.utils.PropertiesBuilder

object StreamsApp extends App {
  val properties = PropertiesBuilder.fromResource("/eidos.streams.properties").get
  val inputTopic = properties.getProperty("input.topic")
  val outputTopic = properties.getProperty("output.topic")
  val eidosSystem = Counter() // new EidosSystem() // some more things, prime?
  val stream = new EidosStream(inputTopic, outputTopic, properties, eidosSystem)

  stream.start()

  sys.ShutdownHookThread {
    stream.close()
  }
}
