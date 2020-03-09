package ai.lum.eidos.kafka.apps.eidos

import java.util.Scanner

import ai.lum.eidos.kafka.stream.EidosStream
import ai.lum.eidos.kafka.utils.Counter
import ai.lum.eidos.kafka.utils.EidosSystem
import ai.lum.eidos.kafka.utils.PropertiesBuilder

object StreamsApp extends App {
  val scanner = new Scanner(System.in)

  new Thread {
    override def run(): Unit = {
      val properties = PropertiesBuilder.fromResource("/eidos.streams.properties").get
      val inputTopic = properties.getProperty("input.topic")
      val outputTopic = properties.getProperty("output.topic")
      val eidosSystem = Counter() // new EidosSystem() // some more things, prime?
      val stream = new EidosStream(inputTopic, outputTopic, properties, eidosSystem)

      sys.ShutdownHookThread {
        stream.close()
      }
      stream.start()
    }
  }.start
  println("Press ENTER to exit...")
  scanner.nextLine()
  System.exit(0)
}
