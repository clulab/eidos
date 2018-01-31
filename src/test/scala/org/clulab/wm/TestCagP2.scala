package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP2 extends Test {
  
  { // S1
    val tester = new Tester(p2s1)

    val economicCrisis = newNodeSpec("South Sudan's economic crisis")
    val sudanesePound = newNodeSpec("value of the South Sudanese Pound", newDecrease("depreciating", "rapidly"))
    val hardCurrency = newNodeSpec("hard currency", newDecrease("shortages"))
    val oilPrices = newNodeSpec("oil prices", newDecrease("declines"))
    val dependenceOnImports = newNodeSpec("dependence on imports", newQuantification("significant"))
    
    behavior of "p2s1"
    
    failingTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(newEdgeSpec(sudanesePound, Causal, economicCrisis)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Zheng) in {
      tester.test(newEdgeSpec(hardCurrency, Causal, economicCrisis)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Zheng) in {
      tester.test(newEdgeSpec(oilPrices, Causal, economicCrisis)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Zheng) in {
      tester.test(newEdgeSpec(dependenceOnImports, Causal, economicCrisis)) should be (successful)
    }
  }

  { // S2
    val tester = new Tester(p2s2)
  
    val conflict = newNodeSpec("Conflict")
    val insecurity = newNodeSpec("insecurity")
    val marketDisruption = newNodeSpec("market disruption")
    val economic = newNodeSpec("economic", newDecrease("downturn"))
    val cropFailure = newNodeSpec("localized crop", newDecrease("failure"))
    val foodPrices = newNodeSpec("food prices", newIncrease("high", "record"))
    val hunger = newNodeSpec("hunger", newIncrease("spread"))

    behavior of "p2s2"

    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(conflict, Causal, foodPrices)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(insecurity, Causal, foodPrices)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(marketDisruption, Causal, foodPrices)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(economic, Causal, foodPrices)) should be (successful)
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(cropFailure, Causal, foodPrices)) should be (successful)
    }
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(hunger) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p2s3)

    val conflict = newNodeSpec("Conflict")
    val economic = newNodeSpec("economic", newDecrease("decline"))
    val violence = newNodeSpec("violence")
    val displacement = newNodeSpec("displacement")

    behavior of "p2s3"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, violence)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, displacement)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(economic, Causal, violence)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(economic, Causal, displacement)) should be (successful)
    }
  }

  { // S4
    val tester = new Tester(p2s4)

    val violence = newNodeSpec("Violence")
    val livestock = newNodeSpec("livestock") //todo: add more modifications when we have that functionality
    val displacement = newNodeSpec("displacement")
    val delayedPlanting = newNodeSpec("delayed planting") //todo: adjust when more mods available


    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    behavior of "p2s4"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(violence, Causal, livestock)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Fan) in {
      tester.test(newEdgeSpec(displacement, Causal, delayedPlanting)) should be (successful)
    }
  }

  { // S5
    val tester = new Tester(p2s5)
  
    val impactsLivestock = newNodeSpec("impacts on livestock")
    val impactsCrops = newNodeSpec("crops") //fixme: any way to get diff span here with impact but not with livestock?
    // TODO: the entity below is 'livelihoods being decimated' because "being..." is an acl dependency, which modifies nouns
    val livelihoods = newNodeSpec("livelihoods being decimated", newDecrease("decimated"))

    behavior of "p2s5"

    passingTest should "have correct edges 1" taggedAs(Mihai) in {
      tester.test(newEdgeSpec(impactsLivestock, Causal, livelihoods)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Mihai) in {
      tester.test(newEdgeSpec(impactsCrops, Causal, livelihoods)) should be (successful)
    }
  }  
}
