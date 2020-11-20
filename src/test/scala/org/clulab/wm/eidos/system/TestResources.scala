package org.clulab.wm.eidos.system

import java.io.File

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.Language

import scala.io.Source

class TestResources extends Test {
  
  behavior of "resources"

  def test(file: File): Unit = {
    val path = file.getCanonicalPath()

    it should "not have any Unicode characters in " + path in {
      val count = Sourcer.sourceFromFile(file).autoClose { source =>
        source.getLines().zipWithIndex.foldRight(0) { (lineAndLineNo, sum) =>
          val line = lineAndLineNo._1
          val lineNo = lineAndLineNo._2
          val badCharAndIndex = line.zipWithIndex.filter { case (c: Char, index: Int) =>
            (c < 32 || 127 < c) && c != '\r' && c != '\n' && c != '\t'
          }
          val complaints = badCharAndIndex.map { case (c: Char, index: Int) =>
            "'" + c + "' found at index " + index + "."
          }

          complaints.foreach(complaint => println("Line " + (lineNo + 1) + ": " + complaint))
          sum + complaints.size
        }
      }
      count should be (0)
    }
  }
  
  // https://groups.google.com/forum/#!topic/scala-user/WrmYHHzcJPw  
  type Operation = (File) => Unit

  val wantedSuffixes = Seq(".conf", ".yml", ".tsv", ".kb", ".txt")
  val unwantedSuffixes = Seq(
    "300d.txt", "vectors.txt", "_2016.txt", "/portuguese/grammars/triggers.yml",
    "word2idx_file.txt", "SentenceClassifierEvaluation.tsv", "SentenceClassifier.tsv"
  )

  def fileMatches(file: File): Boolean = {
    val canonicalPath = file.getCanonicalPath().replace('\\', '/')

    wantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix)) &&
    !unwantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix))
  }

  def directoryMatches(file: File): Boolean = true
  
  def doOperation(path: String)(operation: Operation): Unit = {
    for (files <- Option(new File(path).listFiles); file <- files) {
        if (file.isFile() && fileMatches(file) && file.getAbsolutePath.contains(Language.ENGLISH))
          operation(file)
        if (file.isDirectory && directoryMatches(file))
          doOperation(file.getAbsolutePath)(operation)
    }
  }
  
  doOperation(new File("./src/main/resources").getCanonicalPath())(test)
}
