package org.clulab.wm.eidos

import org.clulab.wm.eidos.test.TestUtils._

import org.clulab.wm.eidos.text.Affect
import org.clulab.wm.eidos.text.NoEvent

import org.clulab.wm.eidos.text.AntiEdgeSpec
import org.clulab.wm.eidos.text.AntiNodeSpec

import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.EdgeSpec

import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.Unmarked

class TestGraphSpec extends Test {
  val rainfallNode = NodeSpec("in rainfall", Dec("decrease"))
  val povertyNode = NodeSpec("poverty", Inc("increased", "significantly"))
  val manyNode = NodeSpec("many", Unmarked("displaced")) 
  val rainfallPovertyEdge = EdgeSpec(rainfallNode, Affect, povertyNode)
  val rainfallNoEventPovertyEdge = EdgeSpec(rainfallNode, NoEvent, povertyNode)
  
  val antiNode = AntiNodeSpec("few", Inc("decreased", "never"))
  val antiEdge = AntiEdgeSpec(rainfallNode, Affect, povertyNode) 

  "rainfallNode" should "have the correct string representation" in {
    rainfallNode.toString() should be ("[in rainfall|+DEC(decrease)]")    
  }
  
  "povertyNode" should "have the correct string representation" in {
    povertyNode.toString() should be ("[poverty|+INC(increased, Quant: significantly)]")    
  }

  "manyNode" should "have the correct string representation" in {
    manyNode.toString() should be ("[many|+displaced]")
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
}
