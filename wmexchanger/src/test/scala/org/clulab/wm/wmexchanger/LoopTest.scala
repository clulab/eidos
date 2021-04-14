package org.clulab.wm.wmexchanger

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.eidoscommon.utils.Test
import org.clulab.wm.wmexchanger.wmconsumer.KafkaConsumerLoopApp
import org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp
import org.clulab.wm.wmexchanger.wmeidos.EidosLoopApp
import org.clulab.wm.wmexchanger.wmproducer.RestProducerLoopApp

import java.io.File
import java.nio.file.Files

class LoopTest extends Test {
  val threads = 3

  class Dirs(baseDir: String, relInputDirOpt: Option[String], relOutputDirOpt: Option[String], relDoneDirOpt: Option[String], relMockDirOpt: Option[String]) {
    val  inputDirOpt: Option[String] = makeAbs(relInputDirOpt)
    val outputDirOpt: Option[String] = makeAbs(relOutputDirOpt)
    val   doneDirOpt: Option[String] = makeAbs(relDoneDirOpt)
    val   mockDirOpt: Option[String] = makeAbs(relMockDirOpt)

    val allDirOpts: Seq[Option[String]] = Seq(inputDirOpt, outputDirOpt, doneDirOpt, mockDirOpt )

    def makeAbs(dirOpt: Option[String]): Option[String] = dirOpt.map { dir => baseDir + dir }

    def makeExist(dirOpt: Option[String]): Unit = dirOpt.foreach { dir => new File(dir).mkdirs() }

    def makeExist(): Unit = allDirOpts.foreach(makeExist)

    def makeEmpty(dirOpt: Option[String]): Unit = {
      dirOpt.foreach { dir =>
        val files = FileUtils
            .findFiles(dir, "")
            .filter(_.isFile)

        files.foreach { file =>
          file.delete()
        }
      }
    }

    def makeEmpty(): Unit = allDirOpts.foreach(makeEmpty)

    def makeFull(dstDir: String, srcDir: String): Unit = {
      val files = FileUtils.findFiles(srcDir, "")

      files.foreach { file =>
        val dstFile = FileEditor(file).setDir(dstDir).get

        Files.copy(file.toPath, dstFile.toPath)
      }
    }

    def testInputEmpty(): Boolean = {
      val inputIsEmpty = inputDirOpt.forall { inputDir =>
        val files = FileUtils
            .findFiles(inputDir, "")
            .filter(_.isFile)

        files.isEmpty
      }
      val mockIsEmpty = mockDirOpt.forall { mockDir =>
        val files = FileUtils
            .findFiles(mockDir, "")
            .filter(_.isFile)

        files.isEmpty
      }

      inputIsEmpty && mockIsEmpty
    }

    def testOutputDone(inputIds: Set[String]): Boolean = {
      outputDirOpt.forall { outputDir =>
        val outputIds = getFileIds(outputDir)

        inputIds == outputIds
      }
    }
  }

  val baseDir = "../corpora/loop/"
  val kafkaConsumerDirs = new Dirs(baseDir, None,                Some("input/kafka"), None,                     Some("input/kafka/mock"))
  val  restConsumerDirs = new Dirs(baseDir, Some("input/kafka"), Some("input"),       Some("input/kafka/done"), Some("input/mock"))
  val         eidosDirs = new Dirs(baseDir, Some("input"),       Some("output"),      Some("input/done"),       None)
  val  restProducerDirs = new Dirs(baseDir, Some("output"),      None,                Some("output/done"),      None)
  val allDirs = Seq(kafkaConsumerDirs, restConsumerDirs, eidosDirs, restProducerDirs)

  val kafkaResourceDir = "./wmexchanger/src/test/resources/kafkaProducer"
  val  restResourceDir = "./wmexchanger/src/test/resources/restProducer"

  val fileIds: Set[String] = getFileIds(kafkaResourceDir)

  assert(fileIds == getFileIds(restResourceDir))

  def getFileIds(dir: String): Set[String] = {
    val fileIds = FileUtils
        .findFiles(dir, "")
        .map { file => StringUtils.beforeLast(file.getName, '.', all = true) }

    fileIds.toSet
  }

  def prepareDirs(): Unit = {
    allDirs.foreach { dirs: Dirs =>
      dirs.makeExist()
      dirs.makeEmpty()
    }
    kafkaConsumerDirs.makeFull(kafkaConsumerDirs.mockDirOpt.get, kafkaResourceDir)
     restConsumerDirs.makeFull( restConsumerDirs.mockDirOpt.get, restResourceDir)
  }

  def startLoopApps(): Seq[Thread] = {
    val kafkaConsumerThread: Thread = new Thread {
      val args: Array[String] = Array(
        "app.topic=dart.cdr.streaming.updates",
        s"app.outputDir=${kafkaConsumerDirs.outputDirOpt.get}"
      )

      override def run(): Unit = KafkaConsumerLoopApp.main(args)
    }
    val restConsumerThread: Thread = new Thread {
      val args: Array[String] = Array(
        s"${restConsumerDirs.inputDirOpt.get}",
        s"${restConsumerDirs.outputDirOpt.get}",
        s"${restConsumerDirs.doneDirOpt.get}"
      )

      override def run(): Unit = RestConsumerLoopApp.main(args)
    }
    val eidosThread: Thread = new Thread {
      val args: Array[String] = Array(
        s"${eidosDirs.inputDirOpt.get}",
        s"${eidosDirs.outputDirOpt.get}",
        s"${eidosDirs.doneDirOpt.get}",
        threads.toString
      )

      override def run(): Unit = EidosLoopApp.main(args)
    }
    val restProducerThread: Thread = new Thread {
      val args: Array[String] = Array(
        s"${restProducerDirs.inputDirOpt.get}",
        s"${restProducerDirs.doneDirOpt.get}"
      )

      override def run(): Unit = RestProducerLoopApp.main(args)
    }

    val loops = Seq(kafkaConsumerThread, restConsumerThread, eidosThread, restProducerThread)
    loops.foreach(_.start())
    loops
  }

  def waitForCompletion(): Unit = {
    def allEmpty() = allDirs.forall { dirs: Dirs =>
      dirs.testInputEmpty()
    }

    while (!allEmpty())
      Thread.sleep(1000)
  }

  def stopLoopApps(threads: Seq[Thread]): Unit = {

    def anyAlive(): Boolean = {
      threads.foreach { thread =>
        if (thread.isAlive)
          thread.interrupt()
      }
      threads.exists { thread =>
        thread.isAlive
      }
    }

    while (anyAlive())
      Thread.sleep(1000)
  }

  def testDirs(): Unit = {
    allDirs.forall { dirs: Dirs =>
      dirs.testOutputDone(fileIds)
    }
  }

  behavior of "pipeline"

  it should "run to completion" in {
    prepareDirs()
    val apps = startLoopApps()
    waitForCompletion()
    stopLoopApps(apps)
    testDirs()
  }
}
