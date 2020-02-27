package ai.lum.eidos.kafka.stream

import ai.lum.eidos.kafka.utils.Counter
import ai.lum.eidos.kafka.utils.PropertiesBuilder
import ai.lum.eidos.kafka.utils.TopologyBuilder
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.Serdes._
import org.apache.kafka.streams.scala.StreamsBuilder

class EidosStream(eidos: Counter, applicationId: String, bootstrapServers: String, inputTopic: String, outputTopic: String, threads: Integer = 1) {
  val properties = PropertiesBuilder()
      .put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
      .put(StreamsConfig.APPLICATION_ID_CONFIG, applicationId)
      .put(StreamsConfig.NUM_STREAM_THREADS_CONFIG, threads)
      .get

  val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder: StreamsBuilder =>
    streamsBuilder
        .stream[String, String](inputTopic)
        .map { (key: String, value: String) =>
          key -> (value + value)
        }
        .to(outputTopic)
  }.get

  def start(): KafkaStreams = {
    val kafkaStreams: KafkaStreams = new KafkaStreams(topology, properties)

    kafkaStreams.cleanUp()
    kafkaStreams.start()
    kafkaStreams
  }
}
