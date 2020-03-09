package ai.lum.eidos.kafka

import ai.lum.eidos.kafka.producer.ExampleProducer
import net.manub.embeddedkafka.EmbeddedKafka
import net.manub.embeddedkafka.EmbeddedKafkaConfig
import org.apache.kafka.common.serialization.Serdes
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.concurrent.ExecutionContext

class ExampleProducerTest extends FlatSpec with EmbeddedKafka with Matchers {
  val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
  val (serializer, deserializer) = {
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

      new ExampleProducer(testTopic, testKafkaUrl)(executionContext)
  }

  "Example producer" should "successfully send a message" in {
    val testKey = "my-test-key"
    val testValue = "my-test-value"

    withRunningKafka {
      producer.send(testKey + " 0", testValue)
      producer.send(testKey + " 1", testValue)

      val actuals: List[(String, String)] =
        consumeNumberKeyedMessagesFrom(topic = testTopic, 2, autoCommit = true)(config, deserializer, deserializer)

      actuals.zipWithIndex.foreach { case (actual, index) =>
        actual._1 shouldBe testKey + " " + index
        actual._2 shouldBe testValue
      }
    }
  }
}
