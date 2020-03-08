package ai.lum.eidos.kafka.apps.eidos

import ai.lum.eidos.kafka.consumer.EidosConsumer
import ai.lum.eidos.kafka.utils.PropertiesBuilder

object ConsumerApp extends App {
  val properties = PropertiesBuilder.fromResource("/eidos.consumer.properties").get
  val topic = properties.getProperty("topic")
  val outputDir = args(0)
  val consumer = new EidosConsumer(topic, properties, outputDir)

  while (true) {
    consumer.poll()
  }

  sys.ShutdownHookThread {
    consumer.close()
  }
}
