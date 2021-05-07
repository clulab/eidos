package org.clulab.wm.wmexchanger.wmconsumer

trait KafkaConsumerish {
  def poll(duration: Int): Unit
  def close(): Unit
}
