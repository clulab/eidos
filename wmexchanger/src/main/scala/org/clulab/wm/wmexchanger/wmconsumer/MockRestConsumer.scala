package org.clulab.wm.wmexchanger.wmconsumer

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils

import java.io.File

class MockRestConsumer(outputDir: String) extends RestConsumerish {
  val inputDir: String = FileEditor(new File(outputDir)).incName("/mock").get.getAbsolutePath

  def open(): Unit = ()

  def close(): Unit = ()

  def download(file: File): String = {
    val cdr = FileUtils.getTextFromFile(file)

    cdr
  }
}
