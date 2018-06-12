package org.clulab.wm.eidos.system

import java.io.File

import org.clulab.wm.eidos.test.TestUtils._

import scala.io.Source

class TestResources extends Test {
  
  behavior of "resources"

  def test(file: File): Unit = {
    val path = file.getCanonicalPath()
    val stream = Source.fromFile(file, "ascii") // , utf8)

    val contents = stream.mkString
    
    it should "not have any Unicode characters in " + path in {
      val index = contents.indexWhere(c => (c < 32 || 127 < c) && c != '\r' && c != '\n' && c != '\t')
      
      index should be < 0
    }
  }
  
  // https://groups.google.com/forum/#!topic/scala-user/WrmYHHzcJPw  
  type Operation = (File) => Unit

  val wantedSuffixes = Seq(".conf", ".yml", ".tsv", ".kb", ".txt")
  val unwantedSuffixes = Seq("vectors.txt")

  def fileMatches(file: File): Boolean = 
      wantedSuffixes.exists(suffix => file.getCanonicalPath().endsWith(suffix)) &&
      !unwantedSuffixes.exists(suffix => file.getCanonicalPath.endsWith(suffix))
    
  def directoryMatches(file: File): Boolean = true
  
  def doOperation(path: String)(operation: Operation): Unit = {
    for (files <- Option(new File(path).listFiles); file <- files) {
        if (file.isFile() && fileMatches(file))
          operation(file)
        if (file.isDirectory && directoryMatches(file))
          doOperation(file.getAbsolutePath)(operation)
    }
  }
  
  doOperation(new File("./src/main/resources").getCanonicalPath())(test)
}
