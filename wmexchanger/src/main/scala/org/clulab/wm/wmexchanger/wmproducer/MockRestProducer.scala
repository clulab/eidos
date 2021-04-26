package org.clulab.wm.wmexchanger.wmproducer

import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File

class MockRestProducer() extends RestProducerish {

  def open(): Unit = ()

  def close(): Unit = ()

  def upload(file: File): String = {
    StringUtils.beforeLast(file.getName, '.', true)
  }
}
