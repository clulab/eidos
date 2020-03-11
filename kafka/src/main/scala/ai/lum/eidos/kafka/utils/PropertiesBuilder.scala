package ai.lum.eidos.kafka.utils


import java.io.InputStream
import java.util.Properties

import org.clulab.wm.eidos.utils.Closer.AutoCloser

class PropertiesBuilder(properties: Properties) {
  def put(key: AnyRef, value: AnyRef): PropertiesBuilder = {
    properties.put(key, value)
    this
  }

  def get: Properties = properties
}

object PropertiesBuilder {

  def apply(properties: Properties): PropertiesBuilder = new PropertiesBuilder(properties)

  def apply(): PropertiesBuilder = new PropertiesBuilder(new Properties)

  def fromResource(resourceName: String): PropertiesBuilder = {
    val properties = new Properties

    PropertiesBuilder.getClass.getResourceAsStream(resourceName).autoClose { inputStream: InputStream =>
      properties.load(inputStream)
    }

    new PropertiesBuilder(properties)
  }
}