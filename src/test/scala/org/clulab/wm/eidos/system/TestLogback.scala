package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.EidosTest
import org.slf4j.{Logger, LoggerFactory}

class TestLogback extends EidosTest {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  behavior of "logback.xml"
  
  it should "work" in {
    logger.debug("This is debug")
    logger.error("This is error")
    logger.info("This is info")
    logger.trace("This is trace")
    logger.warn("This is warn")
  }
}
