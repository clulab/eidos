package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP2 extends Test {
  
  { // S1
    val tester = new Tester(p2s1)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)
    
    "p2s1" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S2
    val tester = new Tester(p2s2)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    "p2s2" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p2s3)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    "p2s3" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S4
    val tester = new Tester(p2s4)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    "p2s4" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }

  { // S5
    val tester = new Tester(p2s5)
  
    val node1 = newNodeSpec(null)
    val node2 = newNodeSpec(null)

    "p2s5" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(node1, Causal, node2)) should be (successful)
    }
  }  
}
