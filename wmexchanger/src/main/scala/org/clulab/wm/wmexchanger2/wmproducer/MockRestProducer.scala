package org.clulab.wm.wmexchanger2.wmproducer

import java.io.File

class MockRestProducer() extends RestProducerish {

  def open(): Unit = ()

  def close(): Unit = ()

  def upload(file: File, documentId: String, ontologyId: String): String = {
    Thread.sleep(100)
    s"${documentId}_${ontologyId}"
  }
}
