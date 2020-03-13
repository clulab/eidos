package ai.lum.eidos.kafka.stream

import ai.lum.eidos.kafka.utils.TopologyBuilder

import java.time.Duration
import java.util.Properties

import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.Serdes._

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.meta.CdrText

class EidosStream(inputTopic: String, outputTopic: String, properties: Properties, eidosSystem: EidosSystem,
    options: EidosSystem.Options, adjectiveGrounder: EidosAdjectiveGrounder) {

  protected def process(cdr: String): String = {
    val cdrText = CdrText(cdr)
    val annotatedDocument = eidosSystem.extractFromText(cdrText.getText, options, cdrText.getMetadata)
    val jsonld = stringify(new JLDCorpus(annotatedDocument).serialize(adjectiveGrounder), pretty = true)

    jsonld
  }

  protected val stream: KafkaStreams = {
    val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder =>
      streamsBuilder
          .stream[String, String](inputTopic)
          .map { (key: String, cdr: String) =>
            println("Streaming " + key)
            key -> process(cdr)
          }
          .to(outputTopic)
    }.get

    new KafkaStreams(topology, properties)
  }

  def start(): Unit = {
    stream.cleanUp()
    stream.start()
  }

  def close(): Unit = {
    println("Closing stream....")
    stream.close(Duration.ofSeconds(10))
  }
}
