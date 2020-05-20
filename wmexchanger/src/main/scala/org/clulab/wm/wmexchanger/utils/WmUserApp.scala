package org.clulab.wm.wmexchanger.utils

import java.util.Properties

class WmUserApp(val args: Array[String], val resourcePropertiesName: String, val filePropertiesKey: String = WmUserApp.properties) {

  def getPropertiesBuilder: PropertiesBuilder = {
    val resourcePropertiesBuilder = PropertiesBuilder.fromResource(resourcePropertiesName)
    val commandPropertiesBuilder = {
      val propertiesBuilder = PropertiesBuilder()

      args.foreach { arg =>
        val key = StringUtils.beforeFirst(arg, '=', all = false)
        val value = StringUtils.afterFirst(arg, '=', all = false)

        if (key.nonEmpty && value.nonEmpty)
          propertiesBuilder.put(key, value)
      }
      propertiesBuilder
    }
    val filePropertiesBuilder = {
      val fileNameOpt: Option[String] = commandPropertiesBuilder
          .getProperty(WmUserApp.app, filePropertiesKey)
          .orElse(resourcePropertiesBuilder.getProperty(WmUserApp.app, filePropertiesKey))
          .orElse(None)

      fileNameOpt
          .map(PropertiesBuilder.fromFile)
          .getOrElse(PropertiesBuilder())
    }

    PropertiesBuilder()
        .putAll(resourcePropertiesBuilder)
        .putAll(filePropertiesBuilder)
        .putAll(commandPropertiesBuilder)
  }

  val propertiesBuilder: PropertiesBuilder = getPropertiesBuilder
  val appProperties: Properties = propertiesBuilder.filter("app").get
  val kafkaProperties: Properties = propertiesBuilder.filter("kafka").get

  val interactive: Boolean = appProperties.getProperty("interactive").toBoolean
}

object WmUserApp {
  val topic = "topic"
  val app = "app"
  val properties = "app.properties"
}