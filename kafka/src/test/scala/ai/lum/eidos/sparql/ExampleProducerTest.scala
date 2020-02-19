package com.twosixlabs.eidos

import ai.lum.eidos.sparql.producer.ExampleProducer
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.common.serialization.Serdes
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

class ExampleProducerTest extends FlatSpec with EmbeddedKafka with Matchers {
  implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
  implicit val (serializer, deserializer) = {
    val serdes = Serdes.String()

    (serdes.serializer(), serdes.deserializer())
  }
  val kafkaPort = 6308
  implicit val config = {
    val zookeeperPort = 6309

    EmbeddedKafkaConfig(kafkaPort, zookeeperPort)
  }
  val testTopic = "eidos.test"
  val producer: ExampleProducer = {
      val testKafkaUrl = s"localhost:${kafkaPort}"

      new ExampleProducer(testTopic, testKafkaUrl)
  }

  "Example producer" should "successfully send a message" in {
    val testKey = "my-test-key"
    val testValue = "my-test-value"

    withRunningKafka {
      producer.send(testKey, testValue)

      val actual: (String, String) = consumeFirstKeyedMessageFrom(topic = testTopic, autoCommit = true)
      actual._1 shouldBe testKey
      actual._2 shouldBe testValue
    }
  }
}
