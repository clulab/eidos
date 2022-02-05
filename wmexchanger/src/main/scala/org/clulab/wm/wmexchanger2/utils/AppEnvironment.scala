package org.clulab.wm.wmexchanger2.utils

import scala.collection.JavaConverters._

object AppEnvironment {
  val etc = Map(
    "EIDOS_VERSION" -> "",
    "ONTOLOGY_VERSION" -> ""
  )

  def isNotSet(keyValue: (String, String)): Boolean = Option(System.getenv(keyValue._1)).isEmpty

  def setEnv(env: Map[String, String]): Unit = {
//    Environment.setEnv(env.filter(isNotSet).asJava)
//    Environment.setEnv(etc.filter(isNotSet).asJava)
  }
}
