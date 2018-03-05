package org.clulab.wm.eidos.text.raps

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._


class TestRaps1 extends Test {

  { //3 Increase
    val sent20 = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
    val tester = new Tester(sent20)

    val research = NodeSpec("research", Inc("boost"))
    val dev = NodeSpec("development", Inc("boost"))
    val prod = NodeSpec("agricultural production", Inc("boost"))


    behavior of "Raps_sent20"

    passingTest should "have the correct node 1" taggedAs(Heather) in {
      tester.test(research) should be (successful)
    }

    passingTest should "have the correct node 2" taggedAs(Heather) in {
      tester.test(dev) should be (successful)
    }

    passingTest should "have the correct node 3" taggedAs(Heather) in {
      tester.test(prod) should be (successful)
    }

  }

}
