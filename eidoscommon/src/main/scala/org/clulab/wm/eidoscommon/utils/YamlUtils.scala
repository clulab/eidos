package org.clulab.wm.eidoscommon.utils

import java.util.{Map => JMap}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

object YamlUtils {
  val yaml = new Yaml

  def escape(string: String): String = yaml.dump(string)

  def newRules(input: String): Array[JMap[String, String]] = {
    val yaml = new Yaml(new Constructor(classOf[Array[JMap[String, String]]]))
    val rule = yaml.load(input).asInstanceOf[Array[JMap[String, String]]]

    rule
  }
}
