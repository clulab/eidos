package org.clulab.wm.eidos.system

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.utils.Configured
import org.clulab.wm.eidos.test.TestUtils._

class TestEnglishConfig extends Test with Configured {
  
  override def getConf: Config = config
  
  val config = ConfigFactory.load("englishTest")

  behavior of "englishTest.conf"
  
  it should "configure" in {
    val invalidOutgoing: List[String] = config[List[String]]("actions.expander.invalidOutgoing")

    invalidOutgoing.contains("acl:relcl") should be (true)
    invalidOutgoing.contains("nmod:poss") should be (false)
    invalidOutgoing.contains("nmod_worsen") should be (true)
  }
}
