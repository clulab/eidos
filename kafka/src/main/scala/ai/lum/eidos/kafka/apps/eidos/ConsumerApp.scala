package ai.lum.eidos.kafka.apps.eidos

import java.util.Scanner

import ai.lum.eidos.kafka.consumer.EidosConsumer
import ai.lum.eidos.kafka.utils.PropertiesBuilder

import org.clulab.wm.eidos.utils.Closer.AutoCloser

object ConsumerApp extends App {
  val scanner = new Scanner(System.in)
  val thread = {
    val thread = new Thread {
      override def run: Unit = {
        val properties = PropertiesBuilder.fromResource("/eidos.consumer.properties").get
        val topic = properties.getProperty("topic")
        val outputDir = args(0)

        new EidosConsumer(topic, properties, outputDir).autoClose { consumer =>
          while (true)
            consumer.poll()
        }
      }
    }
    thread.start
    thread
  }

  println("Press ENTER to exit...")
  scanner.nextLine()
  thread.interrupt
}
