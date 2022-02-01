package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.json4s.JValue

import java.io.File

class MockRestOntologyConsumer(outputDir: String) extends RestConsumerish {
  val mockDir: String = FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath

  def open(): Unit = ()

  def close(): Unit = ()

  def download(ontologyId: String, jValueOpt: Option[JValue]): String = ???
}
