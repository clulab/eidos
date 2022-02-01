package org.clulab.wm.wmexchanger2.wmconsumer

import org.json4s.JValue

trait RestConsumerish {
  def open(): Unit
  def close(): Unit
  def download(id: String, jValueOpt: Option[JValue] = None): String
}
