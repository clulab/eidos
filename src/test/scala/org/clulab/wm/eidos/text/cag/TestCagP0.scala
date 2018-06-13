package org.clulab.wm.eidos.text.cag

import CAG._

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{Causal, Correlation, SameAs}
import org.clulab.wm.eidos.text.{AntiEdgeSpec, AntiNodeSpec}
import org.clulab.wm.eidos.text.{EdgeSpec, NodeSpec}
import org.clulab.wm.eidos.text.{Dec, Inc, Quant}

class TestCagP0 extends Test {

  {
    val s1 = "The decrease in rainfall caused significantly increased poverty."
    val tester = new Tester(s1)
    
    behavior of "a sentence with a 1:1 edge"
    
    it should "have the correct triples" taggedAs(Somebody) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("significantly increased poverty", Inc("increased", "significantly"))
      val edge = EdgeSpec(rainfall, Causal, poverty)
      
      tester.test(edge) should be (successful)
    }
  }
  
  {
    val s2 = "The decrease in rainfall caused significantly increased poverty and decreased humidity."
    val tester = new Tester(s2)
    
    behavior of "a sentence with a 1:2 edge"
    
    it should "have the correct triples" taggedAs(Keith) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("significantly increased poverty", Inc("increased", "significantly"))
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
    
    it should "have the correct triples" taggedAs(Keith) in {
      val rainfall = NodeSpec("rainfall", Dec("decrease"))
      val poverty = NodeSpec("significantly increased poverty", Inc("increased", "significantly"))
      
      val edge = EdgeSpec(rainfall, Causal, poverty)

      val antiRainfall = AntiNodeSpec("rainfall", Dec("decrease"))
      val antiPoverty = AntiNodeSpec("significantly increased poverty", Inc("increased", "significantly"))
      
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

  {
    val s3 = "Worsening food security trends linked to continued conflict have been compounded by market failure, internal displacement and decreasing humanitarian access."
    val tester = new Tester(s3)

    behavior of "a correlation edge"

    tempBrokenEntitiesTest should "have the correct triples" taggedAs(Keith) in {
      val foodSecurityTrends = NodeSpec("food security trends", Dec("Worsening"), Inc("compounded"))
      val conflict = NodeSpec("continued conflict")
      val noConflict = NodeSpec("uncontinued conflict")

      val edge1 = EdgeSpec(conflict, Correlation, foodSecurityTrends)
      val edge2 = EdgeSpec(foodSecurityTrends, Correlation, conflict)
      val edge3 = EdgeSpec(foodSecurityTrends, Correlation, foodSecurityTrends)
      val edge4 = EdgeSpec(conflict, Correlation, conflict)
      val edge5 = EdgeSpec(noConflict, Correlation, foodSecurityTrends)

      tester.test(edge1) should be (successful)
      tester.test(edge2) should be (successful)
      tester.test(edge3) should not be (successful)
      tester.test(edge4) should not be (successful)
      tester.test(edge5) should not be (successful)

      val antiEdge1 = AntiEdgeSpec(conflict, Correlation, foodSecurityTrends)
      val antiEdge2 = AntiEdgeSpec(foodSecurityTrends, Correlation, conflict)
      val antiEdge3 = AntiEdgeSpec(foodSecurityTrends, Correlation, foodSecurityTrends)
      val antiEdge4 = AntiEdgeSpec(conflict, Correlation, conflict)
      val antiEdge5 = AntiEdgeSpec(conflict, SameAs, foodSecurityTrends)

      tester.test(antiEdge1) should not be (successful)
      tester.test(antiEdge2) should not be (successful)
      tester.test(antiEdge3) should be (successful)
      tester.test(antiEdge4) should be (successful)
      tester.test(antiEdge5) should be (successful)
    }
  }

  {
    val text = "X caused record high above-average rainfall"

    val tester = new Tester(text)

    val x = NodeSpec("X")
    val rainfall1 = NodeSpec("record high above-average rainfall", Quant("above-average", "record", "high"), Inc("high"), Inc("above-average"), Inc("above-average"))
    val rainfall2 = NodeSpec("record high above-average rainfall", Inc("above-average"), Quant("above-average", "record", "high"), Inc("high"), Inc("above-average"))
    val rainfall3 = NodeSpec("record high above-average rainfall", Quant("above-average", "high", "record"), Inc("high"), Inc("above-average"), Inc("above-average"))

    behavior of "text"

    it should "find nodes at all" in {
      tester.test(rainfall1) should be (successful)
    }

    it should "find nodes independent of attachment order" in {
      tester.test(rainfall2) should be (successful)
    }

    it should "find nodes independent of adverb order" in {
      tester.test(rainfall3) should be (successful)
    }
  }
}
