package ai.lum.eidos.kafka.apps.eidos

import java.util.Scanner

import ai.lum.eidos.kafka.consumer.EidosConsumer
import ai.lum.eidos.kafka.utils.PropertiesBuilder

object ConsumerApp extends App {
  val scanner = new Scanner(System.in)

  new Thread {
    override def run: Unit = {
      val properties = PropertiesBuilder.fromResource("/eidos.consumer.properties").get
      val topic = properties.getProperty("topic")
      val outputDir = args(0)
      val consumer = new EidosConsumer(topic, properties, outputDir)

      sys.ShutdownHookThread {
        consumer.close()
      }
      while (true) {
        consumer.poll()
      }
    }
  }.start
  println("Press ENTER to exit...")
  scanner.nextLine()
  System.exit(0)
}
