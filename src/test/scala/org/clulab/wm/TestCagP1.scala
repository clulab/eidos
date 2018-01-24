package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP1 extends Test {
  
  { // S1
    val tester = new Tester(p1s1)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)
    
    behavior of "p1s1"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S2
    val tester = new Tester(p1s2)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    behavior of "p1s2"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p1s3)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    behavior of "p1s3"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S4
    val tester = new Tester(p1s4)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    behavior of "p1s4"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  } 
}
