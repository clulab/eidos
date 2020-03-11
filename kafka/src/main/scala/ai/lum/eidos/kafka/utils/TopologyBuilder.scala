package ai.lum.eidos.kafka.utils

import org.apache.kafka.streams.Topology
import org.apache.kafka.streams.scala.StreamsBuilder

class TopologyBuilder(f: StreamsBuilder => Unit) {

  def get: Topology = {
    val streamsBuilder = new StreamsBuilder()

    f(streamsBuilder)
    streamsBuilder.build()
  }
}

object TopologyBuilder {

  def fromStreamsBuilder(f: StreamsBuilder => Unit): TopologyBuilder = new TopologyBuilder(f)
}
