package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.json4s.JValue

import java.io.File

class MockRestDocumentConsumer(mockDir: String) extends RestConsumerish {
  val document = {
    val mockFile = FileEditor(new File("document.json")).setDir(mockDir).get

    FileUtils.getTextFromFile(mockFile)
  }

  def open(): Unit = ()

  def close(): Unit = ()

  def download(docId: String, jValueOpt: Option[JValue]): String = {
    Thread.sleep(100)
    document
  }
}
