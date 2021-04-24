package org.clulab.wm.wmexchanger.wmproducer

import java.io.File

trait RestProducerish {
  def open(): Unit
  def close(): Unit
  def upload(file: File): String
}
