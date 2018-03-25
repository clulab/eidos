package org.clulab.wm.eidos.text.cag

import CAG._

import org.clulab.wm.eidos.test.TestUtils._

import org.clulab.wm.eidos.text.Causal
import org.clulab.wm.eidos.text.Correlation

import org.clulab.wm.eidos.text.AntiEdgeSpec
import org.clulab.wm.eidos.text.AntiNodeSpec

import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.NodeSpec

import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.Dec

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
      
      val edge1 = EdgeSpec(rainfall, Causal, poverty)
      tester.test(edge1) should be (successful)

      val edge2 = EdgeSpec(rainfall, Causal, humidity)
      tester.test(edge2) should be (successful)
    }
  }
  
  {
    val s1 = "The decrease in rainfall caused significantly increased poverty."
    val tester = new Tester(s1)
    
    behavior of "a sentence with anti nodes and edges"
    
    it should "have the correct triples" taggedAs(Somebody) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("poverty", Inc("increased", "significantly"))
      
      val edge = EdgeSpec(rainfall, Causal, poverty)

      val antiRainfall = AntiNodeSpec("rainfall", Dec("decrease"))
      val antiPoverty = AntiNodeSpec("poverty", Inc("increased", "significantly"))
      
      val notRainfall = AntiNodeSpec("not-rainfall", Dec("decrease"))
      val notPoverty = AntiNodeSpec("not-poverty", Inc("increased", "significantly"))
      
      val antiEdge1 = AntiEdgeSpec(rainfall, Correlation, poverty)
      val antiEdge2 = AntiEdgeSpec(notRainfall, Correlation, notPoverty)
      val notEdge0 = AntiEdgeSpec(rainfall, Causal, poverty) // bad edge
      val notEdge1 = AntiEdgeSpec(antiRainfall, Causal, poverty) // bad cause
      val notEdge2 = AntiEdgeSpec(rainfall, Causal, antiPoverty) // bad effect
      val notEdge3 = AntiEdgeSpec(antiRainfall, Causal, antiPoverty) // bad cause and effect

      
      tester.test(rainfall) should be (successful)
      tester.test(poverty) should be (successful)
      
      tester.test(antiRainfall) should not be (successful)
      tester.test(antiPoverty) should not be (successful)
      
      tester.test(notRainfall) should be (successful)
      tester.test(notPoverty) should be (successful)
      
      tester.test(edge) should be (successful)
      tester.test(antiEdge1) should be (successful)
      tester.test(antiEdge2) should be (successful)
      tester.test(notEdge0) should not be (successful)
      tester.test(notEdge1) should not be (successful)
      tester.test(notEdge2) should not be (successful)
      tester.test(notEdge3) should not be (successful)
    }
  }
}
