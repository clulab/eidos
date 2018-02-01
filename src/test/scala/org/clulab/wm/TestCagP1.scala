package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP1 extends Test {
  
  { // S1
    val tester = new Tester(p1s1)
  
    val foodInsecurityLevels = newNodeSpec("Food insecurity levels") //fixme: add extra mods
    val conflict = newNodeSpec("conflict")
    val economy = newNodeSpec("economy", newDecrease("collapsing"))
    val cerealProduction = newNodeSpec("cereal production", newDecrease("low"), newQuantification("low"))
    val rainfall = newNodeSpec("rainfall in southeastern areas", newDecrease("poor"), newQuantification("poor"))
    val copingCapacities = newNodeSpec("coping capacities", newDecrease("exhaustion"))

    behavior of "p1s1"

    passingTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(conflict, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(economy, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(cerealProduction, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(rainfall, Causal, foodInsecurityLevels)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(copingCapacities, Causal, foodInsecurityLevels)) should be (successful)
    }
  }

  { // S2
    val tester = new Tester(p1s2)
  
    val households = newNodeSpec("households", newIncrease("doubled"))
    val foodConsumption = newNodeSpec("food consumption", newDecrease("poor"))

    behavior of "p1s2"

    failingTest should "have correct singleton node 1" taggedAs(Keith) in {
      tester.test(households) should be (successful)
    }
    failingTest should "have correct singleton node 2" taggedAs(Keith) in {
      tester.test(foodConsumption) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p1s3)
  
    val rainfall = newNodeSpec("Rainfall", newDecrease("deficits"))
    val shock = newNodeSpec("shock", newQuantification("major"))
    val pasture = newNodeSpec("pasture")
    val waterAvailability = newNodeSpec("water availability")
    val foodProduction = newNodeSpec("local food production")

    behavior of "p1s3"

    failingTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(shock, Affect, pasture)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(shock, Affect, waterAvailability)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(shock, Affect, foodProduction)) should be (successful)
    }
    failingTest should "have correct singleton node 1" taggedAs(Egoitz) in {
      tester.test(rainfall) should be (successful)
    }
  }

  { // S4
    val tester = new Tester(p1s4)
  
    val rainfall = newNodeSpec("rainfall", newDecrease("depressed"))

    behavior of "p1s4"

    failingTest should "have correct singleton node 1" taggedAs(Adarsh) in {
      tester.test(rainfall) should be (successful)
    }
  } 
}
