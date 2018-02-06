package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP3 extends Test {
  
  { // S1
    val tester = new Tester(p3s1)

    val foodInsecurity = newNodeSpec("Food insecurity", newQuantification("severe")) // todo: add ability to have 'more'?
    val foodInsecurityDeep = newNodeSpec("Food insecurity", newIncrease("deepening")) // todo: add ability to have 'more'?
    val conflict     = newNodeSpec("conflict")
    val displacement = newNodeSpec("displacement")
    val people       = newNodeSpec("people", newQuantification("vulnerable"))
    
    behavior of "p3s1"
    
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(conflict, Causal, people)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(displacement, Causal, people)) should be (successful)
    }
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(foodInsecurity) should be (successful)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(foodInsecurityDeep) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p3s2)
  
    val impacts = newNodeSpec("impacts of flooding")
    val economic = newNodeSpec("economic", newDecrease("collapse"))
    val conflict = newNodeSpec("conflict")
    val production = newNodeSpec("agricultural production", newDecrease("reduced"))
    val insecurity = newNodeSpec("food insecurity", newQuantification("critical"))
    
    behavior of "p3s2"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(impacts, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(economic, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(conflict, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Egoitz) in {
      tester.test(newEdgeSpec(production, Correlation, insecurity)) should be (successful)
    }
  }
  
  { // S3
    val tester = new Tester(p3s3)
  
    behavior of "p3s3"
  }
  
  { // S4
    val tester = new Tester(p3s4)

    val harvest = newNodeSpec("harvest")
    val prices  = newNodeSpec("food prices", newIncrease("high"))
  
    behavior of "p3s4"

    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(newEdgeSpec(harvest, Affect, prices)) should be (successful)
    }
  }
  
  { // S5
    val tester = new Tester(p3s5)

    val economic    = newNodeSpec("economic", newDecrease("decline"))
    val accessFood  = newNodeSpec("access to staple food", newDecrease("reduction"))
    // becky: with current tokenInterval restrictions, can't have access to clean water (i.e., split interval)
    val accessWater = newNodeSpec("clean water", newDecrease("reduction"))
    val foods       = newNodeSpec("foods", newDecrease("reduction"))
  
    behavior of "p3s5"
    //waiting on "economic" fix from becky. 
    failingTest should "have correct edges 1" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(economic, Causal, accessFood)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(economic, Causal, accessWater)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(economic, Causal, foods)) should be (successful)
    }
  }  
}
