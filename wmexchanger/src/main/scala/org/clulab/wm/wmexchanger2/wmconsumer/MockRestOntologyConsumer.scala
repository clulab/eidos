package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.json4s.JValue

import java.io.File

class MockRestOntologyConsumer(mockDir: String) extends RestConsumerish {
  val ontology = {
    val mockFile = FileEditor(new File("ontology.yml")).setDir(mockDir).get

    FileUtils.getTextFromFile(mockFile)
  }

  def open(): Unit = ()

  def close(): Unit = ()

  def download(ontologyId: String, jValueOpt: Option[JValue]): String = {
    Thread.sleep(100)
    ontology
  }
}
