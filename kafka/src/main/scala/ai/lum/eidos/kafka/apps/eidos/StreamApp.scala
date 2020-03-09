package ai.lum.eidos.kafka.apps.eidos

import java.util.Scanner

import ai.lum.eidos.kafka.stream.EidosStream
import ai.lum.eidos.kafka.utils.PropertiesBuilder

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder

object StreamApp extends App {
  val scanner = new Scanner(System.in)

  new Thread {
    override def run(): Unit = {
      val properties = PropertiesBuilder.fromResource("/eidos.stream.properties").get
      val inputTopic = properties.getProperty("input.topic")
      val outputTopic = properties.getProperty("output.topic")
      val config = EidosSystem.defaultConfig
      val eidosSystem = new EidosSystem(config)
      val options = EidosSystem.Options()
      val adjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)
      val stream = {
        val stream = new EidosStream(inputTopic, outputTopic, properties, eidosSystem, options, adjectiveGrounder)

        sys.ShutdownHookThread { stream.close() }
        stream
      }

      stream.start()
    }
  }.start
  println("Press ENTER to exit...")
  scanner.nextLine()
  System.exit(0)
}
