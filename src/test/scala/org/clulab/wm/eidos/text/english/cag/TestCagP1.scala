package org.clulab.wm.eidos.text.english.cag

import CAG._

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._


class TestCagP1 extends EnglishTest {
  
  { // S1
    val tester = new GraphTester(p1s1)
  
    val foodInsecurityLevels = NodeSpec("Food insecurity levels", Quant("extremely alarming")) //fixme: add extra mods
    val conflict = NodeSpec("conflict")
    val economy = NodeSpec("economy", Dec("collapsing"))
    val cerealProduction = NodeSpec("cereal production", Dec("low"), Quant("low"))
    val rainfall = NodeSpec("poor rainfall in southeastern areas", Dec("poor"), Quant("poor"))
    val copingCapacities = NodeSpec("coping capacities after several years of crisis", Dec("exhaustion"))

    behavior of "p1s1"

    tempBrokenEntitiesTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(conflict, Causal, foodInsecurityLevels)) should be (successful)
    }
    tempBrokenEntitiesTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(economy, Causal, foodInsecurityLevels)) should be (successful)
    }
    tempBrokenEntitiesTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(cerealProduction, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(copingCapacities, Causal, foodInsecurityLevels)) should be (successful)
    }
  }

  { // S2
    val tester = new GraphTester(p1s2)
  
    val households = NodeSpec("households", Inc("doubled"))
    val foodConsumption = NodeSpec("food consumption", Dec("poor"), Quant("poor"))

    behavior of "p1s2"

    passingTest should "have correct singleton node 1" taggedAs(Keith) in {
      tester.test(households) should be (successful)
    }
    passingTest should "have correct singleton node 2" taggedAs(Keith) in {
      tester.test(foodConsumption) should be (successful)
    }
  }

  { // S3
    val tester = new GraphTester(p1s3)
  
    val rainfall = NodeSpec("Rainfall", Dec("deficits"))
    val shock = NodeSpec("shock", Quant("major"))
    val pasture = NodeSpec("pasture")
    val waterAvailability = NodeSpec("water availability")
    val foodProduction = NodeSpec("local food production")

    behavior of "p1s3"

    affectEventTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(shock, Causal, pasture)) should be (successful)
    }
    affectEventTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(shock, Causal, waterAvailability)) should be (successful)
    }
    affectEventTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(shock, Causal, foodProduction)) should be (successful)
    }
    passingTest should "have correct singleton node 1" taggedAs(Egoitz) in {
      tester.test(rainfall) should be (successful)
    }
  }

  { // S4
    val tester = new GraphTester(p1s4)
  
    val rainfall = NodeSpec("rainfall", Dec("depressed"))

    behavior of "p1s4"

    passingTest should "have correct singleton node 1" taggedAs(Adarsh) in {
      tester.test(rainfall) should be (successful)
    }
  } 
}
