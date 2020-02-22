package ai.lum.eidos.kafka.stream

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import ai.lum.eidos.kafka.utils.TopologyBuilder
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.Serdes._
import org.apache.kafka.streams.scala.StreamsBuilder

class ExampleStream(applicationId: String, bootstrapServers: String, inputTopic: String, outputTopic: String) {
  val properties = PropertiesBuilder()
      .put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
      .put(StreamsConfig.APPLICATION_ID_CONFIG, applicationId)
      .get
  val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder: StreamsBuilder =>
    streamsBuilder
        .stream[String, String](inputTopic)
        .flatMapValues(textLine => textLine.toLowerCase.split("\\W+"))
        .groupBy((_, word) => word)
        .count()
        .toStream
        .to(outputTopic)
  }.get

  def start(): KafkaStreams = {
    val kafkaStreams: KafkaStreams = new KafkaStreams(topology, properties)

    kafkaStreams.cleanUp()
    kafkaStreams.start()
    kafkaStreams
  }
}
