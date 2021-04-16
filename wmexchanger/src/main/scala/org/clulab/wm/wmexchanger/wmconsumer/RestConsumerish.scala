package org.clulab.wm.wmexchanger.wmconsumer

import java.io.File

trait RestConsumerish {
  def open(): Unit
  def close(): Unit
  def download(file: File): String
}
