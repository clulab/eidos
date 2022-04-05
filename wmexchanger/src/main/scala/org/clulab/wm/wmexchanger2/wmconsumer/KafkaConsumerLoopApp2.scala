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

class KafkaConsumerLoopApp2(args: Array[String]) {
  var useReal: Boolean = KafkaConsumerLoopApp2.useReal

  private val config : Config = ConfigFactory.defaultApplication().resolve()
  private val appProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.app")).get
  private val kafkaProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.consumer")).get

  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  val outputDir: String = appProperties.getProperty("output.dir")
  FileUtils.ensureDirsExist(outputDir)
  val outputDistinguisher: Counter = FileName.getDistinguisher(KafkaConsumerLoopApp2.outputStage, FileUtils.findFiles(outputDir, Extensions.json))

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp2.logger, interactive, waitDuration) {

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
      val consumer =
          if (useReal)
            new RealKafkaConsumer(appProperties, kafkaProperties, KafkaConsumerLoopApp2.outputStage, outputDistinguisher)
          else {
            val inputDir = Option(System.getenv("KAFKA_CONSUMER_MOCK_DIR"))
                .getOrElse(FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath)

            new MockKafkaConsumer(inputDir, outputDir, KafkaConsumerLoopApp2.outputStage, outputDistinguisher)
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

object KafkaConsumerLoopApp2 extends LoopApp {
  var useReal: Boolean = DevtimeConfig.useReal

  // These will be used for the distinguishers and are their indexes.
  val inputStage: Int = Stages.kafkaConsumerInputStage
  val outputStage: Int = Stages.kafkaConsumerOutputStage

  def main(args: Array[String]): Unit = {
    if (false) { // This only seems to work for Windows.
      val password = getPassword()

      AppEnvironment.setEnv {
        val baseDir = "../corpora/feb2022exp1"
        Map(
          "KAFKA_HOSTNAME" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com",
          "KAFKA_CONSUMER_BOOTSTRAP_SERVERS" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com:9093",
          "KAFKA_CONSUMER_SASL_JAAS_CONFIG" -> s"""org.apache.kafka.common.security.plain.PlainLoginModule required username="eidos" password="$password";""",
          "KAFKA_APP_TOPIC" -> "dart.cdr.streaming.updates",

          "KAFKA_CONSUMER_OUTPUT_DIR" -> s"$baseDir/kafkaconsumer/output",
          "KAFKA_CONSUMER_MOCK_DIR" -> s"$baseDir/kafkaconsumer/mock"
        )
      }
    }

    loop {
      () => new KafkaConsumerLoopApp2(args).thread
    }
  }
}
