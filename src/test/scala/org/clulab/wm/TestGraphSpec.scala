package org.clulab.wm

import TestUtils._

class TestGraphSpec extends AgroTest {
  val rainfallNode = newNodeSpec("in rainfall", newDecrease("decrease"))
  val povertyNode = newNodeSpec("poverty", newIncrease("increased", "significantly"))
  val rainfallPovertyEdge = newEdgeSpec(rainfallNode, povertyNode)

  "rainfallNode" should "have the correct string representation" in {
    rainfallNode.toString() should be ("[in rainfall|+DEC(decrease)]")    
  }
  
  "povertyNode" should "have the correct string representation" in {
    povertyNode.toString() should be ("[poverty|+INC(increased, Quant: significantly)]")    
  }
}
