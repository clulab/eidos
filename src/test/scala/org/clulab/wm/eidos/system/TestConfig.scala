package org.clulab.wm.eidos.text.english.cag

import org.clulab.wm.eidos.test.TestUtils._

import org.clulab.utils.Configured
import com.typesafe.config.{Config, ConfigFactory}

class TestConfig extends Test with Configured {
  
  override def getConf: Config = config
  
  var config = ConfigFactory.load("eidosTest")

  behavior of "eidosTest.conf"
  
  it should "configure" in {
    val testTrue = getArgBoolean("EidosSystem.testTrue", Option(false))
    testTrue should be (true)
    
    val testFalse = getArgBoolean("EidosSystem.testFalse", Option(true))
    testFalse should be (false)
    
    val testNotTrue = getArgBoolean("EidosSystem.testNotTrue", Option(true))
    testNotTrue should be (true)

    val testNotFalse = getArgBoolean("EidosSystem.testNotFalse", Option(false))
    testNotFalse should be (false)
    
    val testInt = getArgInt("EidosSystem.testInt", Option(6))
    testInt should be (5)
    
    val testNotInt = getArgInt("EidosSystem.testNotInt", Option(3))
    testNotInt should be (3)
    
    val innerConfig = config.getConfig("EidosSystem.InnerConfig")
    val innerTestInt = innerConfig.getInt("innerTestInt")
    innerTestInt should be (6)
  }
}
