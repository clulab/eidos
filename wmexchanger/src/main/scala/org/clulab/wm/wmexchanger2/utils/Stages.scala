package org.clulab.wm.wmexchanger2.utils

object Stages {
  val kafkaConsumerInputStage = -1
  val kafkaConsumerOutputStage = 0

  val restConsumerInputStage = 0
  val restConsumerOutputStage = 1

  val eidosInputStage = 1
  val eidosOutputStage = 2

  val restProducerInputStage = 2
  val restProducerOutputStage = 3
}
