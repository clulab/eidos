package org.clulab.wm.wmexchanger2.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.{DevtimeConfig, LoopApp, SafeThread}
import org.clulab.wm.wmexchanger2.utils.AppEnvironment
import org.clulab.wm.wmexchanger2.utils.FileName
import org.clulab.wm.wmexchanger2.utils.Stages

import java.io.File
import java.util.Properties

class KafkaConsumerLoopApp(args: Array[String]) {
  var useReal = KafkaConsumerLoopApp.useReal

  private val config : Config = ConfigFactory.defaultApplication().resolve()
  private val appProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.app")).get
  private val kafkaProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.consumer")).get

  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  val outputDir = appProperties.getProperty("output.dir")
  val outputDistinguisher = FileName.getDistinguisher(KafkaConsumerLoopApp.outputStage, FileUtils.findFiles(outputDir, Extensions.json))

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp.logger, interactive, waitDuration) {

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
      val consumer =
          if (useReal)
            new RealKafkaConsumer(appProperties, kafkaProperties, KafkaConsumerLoopApp.outputStage, outputDistinguisher)
          else {
            val inputDir = Option(System.getenv("KAFKA_CONSUMER_MOCK_DIR"))
                .getOrElse(FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath)

            new MockKafkaConsumer(inputDir, outputDir, KafkaConsumerLoopApp.outputStage, outputDistinguisher)
          }

      def close(): Unit = consumer.close()

      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { close() }

      while (!isInterrupted) {
        consumer.poll(pollDuration)
      }
      close()
    }
  }
}

object KafkaConsumerLoopApp extends LoopApp {
  var useReal = DevtimeConfig.useReal

  // These will be used for the distinguishers and are their indexes.
  val inputStage = Stages.kafkaConsumerInputStage
  val outputStage = Stages.kafkaConsumerOutputStage

  def main(args: Array[String]): Unit = {
    val password = args.lift(0).getOrElse("<eidos_password>")

    AppEnvironment.setEnv(
      Map(
        "KAFKA_HOSTNAME" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com",
        "KAFKA_CONSUMER_BOOTSTRAP_SERVERS" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com:9093",
        "KAFKA_CONSUMER_SASL_JAAS_CONFIG" -> s"""org.apache.kafka.common.security.plain.PlainLoginModule required username="eidos" password="$password";""",
        "KAFKA_APP_TOPIC" -> "dart.cdr.streaming.updates",
        "KAFKA_CONSUMER_OUTPUT_DIR" -> "../corpora/feb2022exp_mock/kafkaconsumer/output",
        "KAFKA_CONSUMER_MOCK_DIR" -> "../corpora/feb2022exp_mock/kafkaconsumer/mock"
      )
    )

    loop {
      () => new KafkaConsumerLoopApp(args).thread
    }
  }
}
