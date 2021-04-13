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

  class Dirs(baseDir: String, relInputDirOpt: Option[String], relOutputDirOpt: Option[String], relDoneDirOpt: Option[String], relMockDirOpt: Option[String]) {
    val  inputDirOpt: Option[String] = makeAbs(relInputDirOpt)
    val outputDirOpt: Option[String] = makeAbs(relOutputDirOpt)
    val   doneDirOpt: Option[String] = makeAbs(relDoneDirOpt)
    val   mockDirOpt: Option[String] = makeAbs(relMockDirOpt)

    val allDirOpts: Seq[Option[String]] = Seq(inputDirOpt, outputDirOpt, doneDirOpt, mockDirOpt )

    def makeAbs(dirOpt: Option[String]): Option[String] = dirOpt.map { dir => baseDir + dir }

    def makeExist(dirOpt: Option[String]): Unit = dirOpt.foreach(dir => new File(dir).mkdirs())

    def makeExist(): Unit = allDirOpts.foreach(makeExist)

    def makeEmpty(dirOpt: Option[String]): Unit = {
      dirOpt.foreach { dir =>
        val files = FileUtils.findFiles(dir, "")

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
      inputDirOpt.map { inputDir =>
        val files = FileUtils.findFiles(inputDir, "")

        files.isEmpty
      }.getOrElse(true)
    }

    def testOutputDone(inputIds: Seq[String]): Boolean = {
      outputDirOpt.map { outputDir =>
        val outputIds = getFileIds(outputDir)

        inputIds.toSet == outputIds.toSet
      }.getOrElse(true)
    }
  }

  val baseDir = "../corpora/loop/"
  val kafkaConsumerDirs = new Dirs(baseDir, None,                Some("input/kafka"), None,                     Some("input/kafka/mock"))
  val  restConsumerDirs = new Dirs(baseDir, Some("input/kafka"), Some("input"),       Some("input/kafka/done"), Some("input/mock"))
  val         eidosDirs = new Dirs(baseDir, Some("input"),       Some("output"),      Some("input/done"),       None)
  val  restProducerDirs = new Dirs(baseDir, Some("output"),      None,                Some("output/done"),      None)
  val allDirs = Seq(kafkaConsumerDirs, restConsumerDirs, eidosDirs, restProducerDirs)

  val kafkaResourceDir = "./wmexchanger/src/test/resources/kafkaProducer"
  val  restResourceDir = "./wmexchanger/arc/test/resources/restProducer"

  val fileIds = getFileIds(kafkaResourceDir)

  assert(fileIds.toSet == getFileIds(restResourceDir).toSet)

  def getFileIds(dir: String): Seq[String] = {
    val fileIds = FileUtils
        .findFiles(dir, "")
        .map { file => StringUtils.beforeLast(file.getName, '.', true) }

    fileIds
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
    // How can I get the args in there?

    val kafkaConsumerLoopApp = KafkaConsumerLoopApp(Array()) // how to set args?
//    val restConsumerLoopApp = RestConsumerLoopApp(Array())
//    val eidosLoopApp = EidosLoopApp(Array())
//    val restProducerLoopApp = RestProducerLoopApp(Array())

    Seq.empty[Thread]
  }

  def waitForCompletion(): Unit = {
    allDirs.forall { dirs: Dirs =>
      dirs.testInputEmpty()
    }
  }

  def stopLoopApps(threads: Seq[Thread]): Unit = {

    def anyAlive() = {
      threads.find { thread =>
        if (thread.isAlive)
          thread.interrupt()
        thread.isAlive
      }.nonEmpty
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
