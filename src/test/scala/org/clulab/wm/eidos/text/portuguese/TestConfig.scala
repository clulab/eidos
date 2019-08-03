package org.clulab.wm.eidos.text.portuguese

import org.clulab.wm.eidos.test.TestUtils._

class TestConfig extends PortugueseTest {

  behavior of "config"

  it should "be set for Portuguese" in {
    ieSystem.components.language should be ("portuguese")
  }
}
