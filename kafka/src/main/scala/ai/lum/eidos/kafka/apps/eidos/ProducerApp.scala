package ai.lum.eidos.kafka.apps.eidos

import ai.lum.eidos.kafka.producer.EidosProducer
import ai.lum.eidos.kafka.utils.PropertiesBuilder

import org.clulab.wm.eidos.utils.FileUtils

object ProducerApp extends App {
  val properties = PropertiesBuilder.fromResource("/eidos.producer.properties").get
  val topic = properties.getProperty("topic")
  val producer = new EidosProducer(topic, properties)
  val inputDir = args(0)

  FileUtils.findFiles(inputDir, ".json").foreach { file =>
    producer.send(file)
  }
  producer.close()
}
