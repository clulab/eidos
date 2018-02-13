package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP3 extends Test {
  
  { // S1
    val tester = new Tester(p3s1)

    val foodInsecurity = NodeSpec("Food insecurity", Inc("deepening"), Quant("severe")) // todo: add ability to have 'more'?
    val conflict     = NodeSpec("conflict")
    val displacement = NodeSpec("displacement")
    val people       = NodeSpec("people", Quant("vulnerable"))
    
    behavior of "p3s1"

    passingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, people)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(displacement, Causal, people)) should be (successful)
    }
    passingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(foodInsecurity) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p3s2)
  
    val impacts = NodeSpec("impacts of flooding")
    val economic = NodeSpec("economic", Dec("collapse"))
    val conflict = NodeSpec("conflict")
    val production = NodeSpec("agricultural production", Dec("reduced"))
    val insecurity = NodeSpec("food insecurity", Quant("critical"))
    
    behavior of "p3s2"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(impacts, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(economic, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(conflict, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(production, Correlation, insecurity)) should be (successful)
    }
  }
  
  { // S3
    val tester = new Tester(p3s3)
  
    behavior of "p3s3"
  }
  
  { // S4
    val tester = new Tester(p3s4)

    val harvest = NodeSpec("harvest")
    val prices  = NodeSpec("food prices", Inc("high"))
  
    behavior of "p3s4"

    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(harvest, Affect, prices)) should be (successful)
    }
  }
  
  { // S5
    val tester = new Tester(p3s5)

    val economic    = NodeSpec("economic", Dec("decline"))
    val accessFood  = NodeSpec("access to staple food", Dec("reduction"))
    // becky: with current tokenInterval restrictions, can't have access to clean water (i.e., split interval)
    val accessWater = NodeSpec("clean water", Dec("reduction"))
    val foods       = NodeSpec("foods", Dec("reduction"))
  
    behavior of "p3s5"
    //waiting on "economic" fix from becky. 

    passingTest should "have correct edges 1" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(economic, Causal, accessFood)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(economic, Causal, accessWater)) should be (successful)

    }
    //becky said this might need more modifications since there is a det:qmod for "variety of foods"
    failingTest should "have correct edges 3" taggedAs(Mithun) in {
      tester.test(EdgeSpec(economic, Causal, foods)) should be (successful)
    }
  }  
}
