package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP3 extends Test {
  
  { // S1
    val tester = new Tester(p3s1)
  
    val conflict     = newNodeSpec("conflict")
    val displacement = newNodeSpec("displacement")
    val people       = newNodeSpec("people", newQuantification("vulnerable"))
    
    "p3s1" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, people)) should be (successful)
    }
    ignore should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(displacement, Causal, people)) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p3s2)
  
    val impacts = newNodeSpec("impacts of flooding")
    val economic = newNodeSpec("economic", newDecrease("collapse"))
    val conflict = newNodeSpec("conflict")
    val production = newNodeSpec("agricultural production", newDecrease("reduced"))
    val insecurity = newNodeSpec("food insecurity", newQuantification("critical"))
    
    "p3s2" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(impacts, Causal, production)) should be (successful)
    }
    ignore should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(economic, Causal, production)) should be (successful)
    }
    ignore should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, production)) should be (successful)
    }
    ignore should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(production, Correlation, insecurity)) should be (successful)
    }
  }
  
  { // S3
    val tester = new Tester(p3s3)
  
    "p3s3" should "not be ignored" in {
    }
  }
  
  { // S4
    val tester = new Tester(p3s4)

    val harvest = newNodeSpec("harvest")
    val prices  = newNodeSpec("food prices", newIncrease("high"))
  
    "p3s4" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(harvest, Affect, prices)) should be (successful)
    }
  }
  
  { // S5
    val tester = new Tester(p3s5)

    val economic    = newNodeSpec("economic", newDecrease("decline"))
    val accessFood  = newNodeSpec("access to staple foods", newDecrease("reduction"))
    val accessWater = newNodeSpec("access to clean water", newDecrease("reduction"))
    val foods       = newNodeSpec("foods", newDecrease("reduction"))
  
    "p3s5" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(economic, Causal, accessFood)) should be (successful)
    }
    ignore should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(economic, Causal, accessWater)) should be (successful)
    }
    ignore should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(economic, Causal, foods)) should be (successful)
    }
  }  
}
