package org.clulab.wm.eidos.utils

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config

object Domain {
  protected val DOMAIN_WM = "wm"
  protected val DOMAIN_CAUSEEX = "causeex"

  protected var configOpt: Option[Config] = None
  protected var domainOpt: Option[String] = None

  def setConfig(config: Config): Unit = {
    configOpt = Some(config)
    domainOpt = Some(getDomain(config))
  }

  def getDomain(config: Config): String = {
    config[String]("EidosSystem.domain")
  }

  def getDomain: Option[String] = domainOpt

  def isCauseEx: Boolean = domainOpt.get == DOMAIN_CAUSEEX

  def isCx: Boolean = isCauseEx

  def isWm: Boolean = domainOpt.get == DOMAIN_WM
}
