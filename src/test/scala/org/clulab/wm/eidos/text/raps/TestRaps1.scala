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

  {
    //Unsure exactly what this sentences convey. Are there actually any causal events here?
    val sent21 = "A combination of increasing population, government plans to invest in fertilizer factory, " +
      "government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, " +
      "government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments " +
      "expected to change agriculture development in the country."
    val tester = new Tester(sent21)

    val population = NodeSpec("population", Inc("increasing"))
    val econ = NodeSpec("economic performance", Inc("improved"))

    behavior of "Raps_sent21"

    passingTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(population) should be (successful)
    }

    //currently extracts "economic performance expected" as the entity text
    failingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(econ) should be (successful)
    }

  }


}
