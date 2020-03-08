package ai.lum.eidos.kafka.stream

import ai.lum.eidos.kafka.utils.Counter
import ai.lum.eidos.kafka.utils.EidosSystem
import ai.lum.eidos.kafka.utils.TopologyBuilder

import java.time.Duration
import java.util.Properties

import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.Serdes._
import org.apache.kafka.streams.scala.StreamsBuilder

class EidosStream(inputTopic: String, outputTopic: String, properties: Properties, eidos: Counter) {
  val streams = {
    val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder: StreamsBuilder =>
      streamsBuilder
          .stream[String, String](inputTopic)
          .map { (id: String, cdr: String) =>
            id -> cdr // eidos.process(cdr)
          }
          .to(outputTopic)
    }.get

    new KafkaStreams(topology, properties)
  }

  def start(): Unit = {
    streams.cleanUp()
    streams.start()
  }

  def close(): Unit = {
    streams.close(Duration.ofSeconds(10))
  }
}
