package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP1 extends Test {
  val tester = new Tester(p1s1)
  "p1s1" should "have correct nodes" in {
    val poorRainfall = newNodeSpec("poor rainfall in southeastern areas", newQuantification("poor"))
    val cerealProduction = newNodeSpec("cereal production", newQuantification("low"), newDecrease("low"))
    
    tester.test(poorRainfall) shouldBe (successful)
    tester.test(cerealProduction) shouldBe (successful)
  }
  
  // These are examples from the entire paragraph, just for syntax
  "p1" should "have tests that compile" in {
    val conflict = newNodeSpec("conflict")
    val economy = newNodeSpec("economy", newDecrease("collapsing"))
    val foodInsecurityLevels = newNodeSpec("food insecurity levels") // what is +alarming(extremely)?
    val cerealProduction = newNodeSpec("cereal production", newDecrease("low"))
    val copingCapacities = newNodeSpec("coping capacities", newDecrease("exhaustion"))
    val rainfall = newNodeSpec("rainfall", newDecrease("poor"))
    
    // This will be OutEdge and then will also test for InEdge
    // Alternatively, add to Node the number of expected edges in and out?
     val conflict_foodInsecurityLevelsEdge = newEdgeSpec(conflict, Causal, foodInsecurityLevels)
     // conflict_foodInsecurityLevelsEdge.test(mentions) shouldBe successful
     
     val economy_foodInsecurityLevelsEdge = newEdgeSpec(economy, Causal, foodInsecurityLevels)
     // economy_foodInsecurityLevelsEdge.test(mentions) shouldBe successful
  }
}
