package org.clulab.wm.wmexchanger2.wmproducer

import java.io.File

class MockRestProducer() extends RestProducerish {

  def open(): Unit = ()

  def close(): Unit = ()

  def upload(file: File, documentId: String, ontologyId: String): String = {
    s"${documentId}_${ontologyId}"
  }
}
