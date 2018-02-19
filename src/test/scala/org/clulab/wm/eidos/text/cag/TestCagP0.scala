package org.clulab.wm.eidos.text.cag

import CAG._

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.Causal
import org.scalactic.source.Position.apply

class TestCagP0 extends Test {

  {
    val s1 = "The decrease in rainfall caused significantly increased poverty."
    val tester = new Tester(s1)
    
    behavior of "a sentence with a 1:1 edge"
    
    it should "have the correct triples" taggedAs(Somebody) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("poverty", Inc("increased", "significantly"))
      val edge = EdgeSpec(rainfall, Causal, poverty)
      
      tester.test(edge) should be (successful)
    }
  }
  
  {
    val s2 = "The decrease in rainfall caused significantly increased poverty and decreased humidity."
    val tester = new Tester(s2)
    
    behavior of "a sentence with a 1:2 edge"
    
    it should "have the correct triples" taggedAs(Somebody) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("poverty", Inc("increased", "significantly"))
      val humidity = NodeSpec("humidity", Dec("decreased"))
      
      val edge = EdgeSpec(rainfall, Causal, poverty, humidity)
      
      tester.test(edge) should be (successful)
    }
  }
}
