package org.clulab.wm.eidos.text

import org.clulab.wm.eidos.test.TestUtils._

class TestImplicitText extends ExtractionTest {

  {
    // This strange text was copied from an actual failing document.
    val text = "READ: Security, Ebola to top agenda at Addis summit"

    behavior of "text ending with implicit terminating period"

    it should "not not crash" in {
      ieSystem.extractFromText(text)
    }
  }
}
