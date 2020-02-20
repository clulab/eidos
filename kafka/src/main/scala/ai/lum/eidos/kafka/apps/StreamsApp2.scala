package ai.lum.eidos.kafka.apps

import java.util.Properties
import java.util.concurrent.TimeUnit

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.apache.kafka.common.serialization.Serde
import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsBuilder
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.Topology
import org.apache.kafka.streams.kstream.KStream
import org.apache.kafka.streams.kstream.KTable
import org.apache.kafka.streams.kstream.Materialized
//import org.apache.kafka.streams.scala.ImplicitConversions._
//import org.apache.kafka.streams.scala.kstream._

object StreamsApp2 {
  import Serdes._
  import collection.JavaConverters._

  val props = new Properties
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "streams-wordcount")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.String.getClass.getName)
  props.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.String.getClass.getName)

  val stringSerde: Serde[String] = Serdes.String()
  val longSerde: Serde[java.lang.Long] = Serdes.Long()

  val builder = new StreamsBuilder()
  val textLines: KStream[String, String] = builder.stream("streams-plaintext-input")

  val topology: Topology = builder.build()

  println(topology.describe())

  val wordCounts: KTable[String, java.lang.Long] = textLines
      .flatMapValues { textLine => textLine.toLowerCase.split("\\W+").toList.asJava }
      .groupBy((_, word: String) => word)
      // this is a stateful computation config to the topology
      .count() // "word-counts")

  wordCounts.toStream.to("streams-wordcount-output")

  val streams = new KafkaStreams(topology, props)
  streams.start()
}
