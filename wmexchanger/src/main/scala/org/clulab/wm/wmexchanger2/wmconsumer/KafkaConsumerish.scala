package org.clulab.wm.wmexchanger2.wmconsumer

trait KafkaConsumerish {
  def poll(duration: Int): Unit
  def close(): Unit
}
