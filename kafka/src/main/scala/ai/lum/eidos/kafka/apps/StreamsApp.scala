package ai.lum.eidos.kafka.apps

import java.util.Properties
import java.util.concurrent.TimeUnit

import ai.lum.eidos.kafka.utils.PropertiesBuilder
import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsBuilder
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.kstream.KStream
import org.apache.kafka.streams.kstream.KTable
import org.apache.kafka.streams.kstream.Materialized
//import org.apache.kafka.ImplicitConversions._

import java.util.Properties
import java.util.concurrent.TimeUnit

import org.apache.kafka.streams.kstream.Materialized
//import org.apache.kafka.streams.scala.ImplicitConversions._
//import org.apache.kafka.streams.scala.kstream._

object StreamsApp {
  import Serdes._
  import collection.JavaConverters._

  val properties: Properties = {
    val className = Serdes.String.getClass.getName

    PropertiesBuilder()
        .put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "kafka-broker1:9092")
        .put(StreamsConfig.APPLICATION_ID_CONFIG, "eidos-stream")
        .put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, className)
        .put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, className)
        .get
  }

  val builder: StreamsBuilder = new StreamsBuilder
  val textLines: KStream[String, String] = builder.stream[String, String]("TextLinesTopic")
  val wordCounts: KTable[String, java.lang.Long] = textLines
      .flatMapValues(textLine => textLine.toLowerCase.split("\\W+").toList.asJava)
      .groupBy((_, word: String) => word)
      .count()//(Materialized.as("counts-store"))
  wordCounts.toStream.to("WordsWithCountsTopic")



//      .groupBy((_, word) => word)
//      .count()(Materialized.as("counts-store")) // the key then
//  wordCounts.toStream.to("WordsWithCountsTopic")

//  val streamsBuilder = new StreamsBuilder()
//      .stream[String, String]("eidos-input")
//      .mapValues { input: String => (input, input.reverse) }
//      .to("eidos-output")

  val streams = new KafkaStreams(builder.build, properties)
  streams.start()

  sys.ShutdownHookThread {
    streams.close(10, TimeUnit.SECONDS)
  }
}
