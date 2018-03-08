package org.clulab.wm.eidos.system

import java.io.File
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Sourcer

class TestResources extends Test {
  
  behavior of "resources"

  def test(file: File): Unit = {
    val path = file.getCanonicalPath()
    val stream = Sourcer.fromFile(path)
    val contents = stream.mkString
    
    it should "not have any Unicode characters in " + path in {
      val index = contents.indexWhere(c => c > 127)
      
      index should be < 0
    }
  }
  
  // https://groups.google.com/forum/#!topic/scala-user/WrmYHHzcJPw  
  type Operation = (File) => Unit

  val suffixes = Seq(".conf", ".yml", ".tsv", ".kb")

  def fileMatches(file: File): Boolean = 
      suffixes.exists(filter => file.getCanonicalPath().endsWith(filter))
    
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
