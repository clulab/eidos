package org.clulab.wm.eidos.graph

import org.clulab.wm.eidos.test.TestUtils._

class TestGraphSpec extends Test {
  val rainfallNode = NodeSpec("in rainfall", Dec("decrease"))
  val povertyNode = NodeSpec("poverty", Inc("increased", "significantly"))
  val manyNode = NodeSpec("many", Unmarked("displaced")) 
  val rainfallPovertyEdge = EdgeSpec(rainfallNode, Affect, povertyNode)
  val rainfallNoEventPovertyEdge = EdgeSpec(rainfallNode, NoEvent, povertyNode)
  
  val antiNode = AntiNodeSpec("few", Inc("decreased", "never"))
  val antiEdge = AntiEdgeSpec(rainfallNode, Affect, povertyNode)
  
  val adverbNode = NodeSpec("diet", Dec("poor", "extremely"), Quant("poor", "extremely"))

  "rainfallNode" should "have the correct string representation" in {
    rainfallNode.toString() should be ("[in rainfall|+DEC(decrease)]")    
  }
  
  "povertyNode" should "have the correct string representation" in {
    povertyNode.toString() should be ("[poverty|+INC(increased, Quant: significantly)]")    
  }

  "manyNode" should "have the correct string representation" in {
    manyNode.toString() should be ("[many|+(displaced)]")
  }
  
  "rainfallPovertyEdge" should "have the correct string representation" in {
    rainfallPovertyEdge.toString() should be ("[in rainfall|+DEC(decrease)]->(Affect)->[poverty|+INC(increased, Quant: significantly)]")
  }

  "rainfallNoEventPovertyEdge" should "have the correct string representation" in {
    rainfallNoEventPovertyEdge.toString() should be ("[in rainfall|+DEC(decrease)]->()->[poverty|+INC(increased, Quant: significantly)]")
  }
  
  "antiNode" should "have the correct string representation" in {
    antiNode.toString() should be ("]few|+INC(decreased, Quant: never)[")
  }
  "antiEdge" should "have the correct string representation" in {
    antiEdge.toString() should be ("[in rainfall|+DEC(decrease)]->)Affect(->[poverty|+INC(increased, Quant: significantly)]")
  }
  
  "adverbs" should "be shown for Quantizations" in {
    adverbNode.toString() should be ("[diet|+DEC(poor, Quant: extremely)+QUANT(poor, Quant: extremely)]")
  }
}
