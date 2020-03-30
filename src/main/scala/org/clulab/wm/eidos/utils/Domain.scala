package org.clulab.wm.eidos.utils

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config

object Domain {
  val DOMAIN_WM = "wm"
  val DOMAIN_CAUSEEX = "causeex"

  def getDomain(config: Config): String = {
    config[String]("EidosSystem.domain")
  }
}
