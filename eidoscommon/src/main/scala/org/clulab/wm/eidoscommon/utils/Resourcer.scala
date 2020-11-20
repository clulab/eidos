package org.clulab.wm.eidoscommon.utils

import java.io.File

import com.typesafe.config.Config

trait Resourceable {
  def getText(resourceName: String): String
}

class Resourcer(val useFile: Boolean) extends Resourceable {

  def getTextFromResource(resourceName: String): String =
    FileUtils.getTextFromResource(resourceName)

  def getText(resourceName: String): String = {
    if (!useFile)
      getTextFromResource(resourceName)
    else {
      // What if it is in src/test?
      val resourceDir = "./src/main/resources"
      val fileName = resourceDir + (if (resourceName.charAt(0) != '/') "/" else "") + resourceName
      val file = new File(fileName)

      if (file.exists())
        FileUtils.getTextFromFile(file)
      else
        getTextFromResource(resourceName)
    }
  }
}

object Resourcer extends Resourceable {
  val defaultInstance: Resourcer = new Resourcer(useFile = false)
  var instance: Resourcer = defaultInstance

  def setConfig(config: Config): Unit = {
    val myConfig = config.getConfig("resourcer")
    val useFile = myConfig.getBoolean("useFile")
    val resourcer = new Resourcer(useFile)

    instance = resourcer
  }

  def getText(resourceName: String): String = instance.getText(resourceName)
}
