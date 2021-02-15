package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.Test
import org.clulab.wm.eidoscommon.utils.Logging

class TestLogback extends Test with Logging {

  behavior of "logback.xml"
  
  it should "work" in {
    logger.debug("This is debug")
    logger.error("This is error")
    logger.info("This is info")
    logger.trace("This is trace")
    logger.warn("This is warn")
  }
}
