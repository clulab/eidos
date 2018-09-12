package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestGlobalAction extends Test {
  
  {
    val text = "Summer storms caused a reduction in rainfall deficits."

    val tester = new Tester(text)
  
    val rainfall = NodeSpec("rainfall deficits", Dec("reduction"), Dec("deficits"))

    behavior of "a sentence requiring global action"

    passingTest should "have correct node" taggedAs(Somebody) in {
      tester.test(rainfall) should be (successful)
    }
  }
}
