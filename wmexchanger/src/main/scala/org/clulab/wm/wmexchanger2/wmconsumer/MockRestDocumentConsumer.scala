package org.clulab.wm.wmexchanger2.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.wmexchanger.utils.Extensions
import org.json4s.JValue

import java.io.File

class MockRestDocumentConsumer(outputDir: String) extends RestConsumerish {
  val mockDir: String = FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath

  def open(): Unit = ()

  def close(): Unit = ()

  def download(docId: String, jValue: JValue): String = {
    val mockFile = new File(mockDir + "/" + docId + Extensions.json).getAbsolutePath
    val cdr = FileUtils.getTextFromFile(mockFile)

    new File(mockFile).delete()
    cdr
  }
}
