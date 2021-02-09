package org.clulab.wm.eidoscommon.utils

import java.util.Properties
import org.clulab.wm.eidoscommon.utils.Closer._

import scala.collection.JavaConverters._

class PropertiesBuilder(properties: Properties) {

  def put(key: String, value: String): PropertiesBuilder = {
    properties.setProperty(key, value)
    this
  }

  def get: Properties = properties

  def getProperty(prefix: String, key: String): Option[String] = Option(properties.getProperty(prefix + "." + key))

  def putAll(propertiesBuilder: PropertiesBuilder): PropertiesBuilder = {
    // The putting is performed iteratively in order to avoid this Java 11 error:
    // [error] both method putAll in class Properties of type (x$1: java.util.Map[_, _])Unit
    // [error] and  method putAll in class Hashtable of type (x$1: java.util.Map[_ <: Object, _ <: Object])Unit
    // It also ensures that all keys and values are strings.
    // properties.putAll(propertiesBuilder.get) // former code
    propertiesBuilder.get.asScala.foreach {
      case (key: String, value: String) => put(key, value)
      case _ =>
    }
    this
  }

  def filter(prefix: String): PropertiesBuilder = {
    val propertiesBuilder = PropertiesBuilder()
    val start = prefix + "."

    properties.asScala.foreach { case (key, value) =>
      if (key.startsWith(start))
        propertiesBuilder.put(key.drop(start.length), value)
    }

    propertiesBuilder
  }
}

object PropertiesBuilder {

  def apply(properties: Properties): PropertiesBuilder = new PropertiesBuilder(properties)

  def apply(): PropertiesBuilder = new PropertiesBuilder(new Properties)

  def fromResource(resourceName: String): PropertiesBuilder = {
    val properties = new Properties

    PropertiesBuilder.getClass.getResourceAsStream(resourceName).autoClose { inputStream =>
      properties.load(inputStream)
    }

    new PropertiesBuilder(properties)
  }

  def fromFile(fileName: String): PropertiesBuilder = {
    val properties = new Properties

    FileUtils.newBufferedInputStream(fileName).autoClose { bufferedInputStream =>
      properties.load(bufferedInputStream)
    }

    new PropertiesBuilder(properties)
  }
}
