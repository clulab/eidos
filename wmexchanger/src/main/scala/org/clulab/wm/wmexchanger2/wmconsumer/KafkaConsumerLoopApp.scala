package org.clulab.wm.wmexchanger2.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.{DevtimeConfig, LoopApp, SafeThread}
import org.clulab.wm.wmexchanger2.utils.AppEnvironment
import org.clulab.wm.wmexchanger2.utils.FileName

import java.util.Properties

class KafkaConsumerLoopApp(args: Array[String]) {
  var useReal = KafkaConsumerLoopApp.useReal

  private val config : Config = ConfigFactory.defaultApplication().resolve()
  private val appProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.app")).get
  private val kafkaProperties : Properties = PropertiesBuilder.fromConfig(config.getConfig("kafka.consumer")).get

  val waitDuration: Long = appProperties.getProperty("wait.duration").toLong
  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
  val pollDuration: Int = appProperties.getProperty("poll.duration").toInt
  // TODO: Move this below so that it only affects the Mock version.
  val outputDir: String = appProperties.getProperty("output.dir")

  val thread: SafeThread = new SafeThread(KafkaConsumerLoopApp.logger, interactive, waitDuration) {
    val outputDistinguisher = Counter(FileName.getDistinguisher(0, FileUtils.findFiles(outputDir, Extensions.json)))

    override def runSafely(): Unit = {
      // This is kept open the entire time, so time between pings is extra important.
      val consumer =
          if (useReal)
            new RealKafkaConsumer(appProperties, kafkaProperties, outputDistinguisher)
          else {
            // In some tests with useReal = false, the outputDir is passed in args.
            val mockOutputDir = LoopApp
                .getNamedArg(args, "app.outputDir")
                .getOrElse(outputDir)
            new MockKafkaConsumer(mockOutputDir, outputDistinguisher)
          }
      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      sys.ShutdownHookThread { consumer.close() }

      while (!isInterrupted) {
        consumer.poll(pollDuration)
      }
      consumer.close()
    }
  }
}

object KafkaConsumerLoopApp extends LoopApp {
  var useReal = DevtimeConfig.useReal

  def main(args: Array[String]): Unit = {
    val password = args.lift(0).getOrElse("<eidos_password>")

    AppEnvironment.setEnv(
      Map(
        "KAFKA_HOSTNAME" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com",
        "KAFKA_CONSUMER_BOOTSTRAP_SERVERS" -> "wm-ingest-pipeline-streaming-1.prod.dart.worldmodelers.com:9093",
        "KAFKA_CONSUMER_SASL_JAAS_CONFIG" -> s"""org.apache.kafka.common.security.plain.PlainLoginModule required username="eidos" password="$password";""",
        "KAFKA_CONSUMER_OUTPUT_DIR" -> "../corpora/wmexchanger2/kafka",
        "KAFKA_APP_TOPIC" -> "dart.cdr.streaming.updates"
      )
    )

    loop {
      () => new KafkaConsumerLoopApp(args).thread
    }
  }
}
