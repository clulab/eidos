package ai.lum.eidos.kafka.utils

import java.util.Properties

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
}