package ai.lum.eidos.kafka.apps

import java.time.Duration

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import ai.lum.eidos.kafka.utils.TopologyBuilder
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.Serdes._
import org.apache.kafka.streams.scala.StreamsBuilder

object StreamsApp extends App {
  val properties = PropertiesBuilder()
      .put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
      .put(StreamsConfig.APPLICATION_ID_CONFIG, "eidos-stream")
      .get
  val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder: StreamsBuilder =>
    streamsBuilder
        .stream[String, String]("eidos-input")
        .flatMapValues(textLine => textLine.toLowerCase.split("\\W+"))
        .groupBy((_, word) => word)
        .count()
        .toStream
        .to("eidos-output")
  }.get
  val streams: KafkaStreams = new KafkaStreams(topology, properties)

  streams.cleanUp()
  streams.start()

  // Add shutdown hook to respond to SIGTERM and gracefully close Kafka Streams
  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
