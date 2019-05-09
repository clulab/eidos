package org.clulab.wm.eidos.system

import java.io.File
import java.io.InputStreamReader

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

class TestCrLf extends Test {
  
  behavior of "resources"

  def test(file: File): Unit = {
    val path = file.getCanonicalPath()
    val buffer = new Array[Char](1024)

    it should "not have any CrLf line endings in " + path in {
      val inputReader = new InputStreamReader(
        FileUtils.newBufferedInputStream(file),
        Sourcer.utf8
      )
      val hasCrLf = inputReader.autoClose { inputReader =>
        var hasCrLf = false
        var endedWithCr = false

        var readCount = inputReader.read(buffer)
        while (!hasCrLf && readCount > 0) {
          hasCrLf |= (endedWithCr && buffer(0) == '\n')
          hasCrLf |= buffer.containsSlice("\r\n")
          endedWithCr = buffer(readCount - 1) == '\r'
          readCount = inputReader.read(buffer)
        }
        hasCrLf
      }

      hasCrLf should be (false)
    }
  }
  
  // https://groups.google.com/forum/#!topic/scala-user/WrmYHHzcJPw  
  type Operation = (File) => Unit

  val wantedSuffixes = Seq(".conf", ".yml", ".tsv", ".kb", ".txt")
  val unwantedSuffixes = Seq.empty[String]

  def fileMatches(file: File): Boolean = {
    val canonicalPath = file.getCanonicalPath().replace('\\', '/')

    wantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix)) &&
    !unwantedSuffixes.exists(suffix => canonicalPath.endsWith(suffix))
  }

  def directoryMatches(file: File): Boolean = true
  
  def doOperation(path: String)(operation: Operation): Unit = {
    for (files <- Option(new File(path).listFiles); file <- files) {
        if (file.isFile() && fileMatches(file) && file.getAbsolutePath.contains("english"))
          operation(file)
        if (file.isDirectory && directoryMatches(file))
          doOperation(file.getAbsolutePath)(operation)
    }
  }
  
  doOperation(new File("./src/main/resources").getCanonicalPath())(test)
}
