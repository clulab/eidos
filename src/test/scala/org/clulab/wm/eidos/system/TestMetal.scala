package org.clulab.wm.eidos.system

import ai.lum.common.ConfigUtils._
import org.clulab.dynet.Metal
import org.clulab.dynet.Utils
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils._

class TestMetal extends Test {
  
  behavior of "Metal"

  it should "load" in {
    val config = EidosSystem.defaultConfig
    val modelFilenamePrefix = config[String]("geonorm.modelFilenamePrefix")
    val metal = {
      Utils.initializeDyNet()
      Metal(modelFilenamePrefix.drop(1))
    }
  }
}
