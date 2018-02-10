package org.clulab.wm

import TestUtils._

class TestGraphSpec extends Test {
  val rainfallNode = NodeSpec("in rainfall", Dec("decrease"))
  val povertyNode = NodeSpec("poverty", Inc("increased", "significantly"))
  val manyNode = NodeSpec("many", Unmarked("displaced")) 
  val rainfallPovertyEdge = EdgeSpec(rainfallNode, Affect, povertyNode)

  "rainfallNode" should "have the correct string representation" in {
    rainfallNode.toString() should be ("[in rainfall|+DEC(decrease)]")    
  }
  
  "povertyNode" should "have the correct string representation" in {
    povertyNode.toString() should be ("[poverty|+INC(increased, Quant: significantly)]")    
  }

  "manyNode" should "have the correct string representation" in {
    manyNode.toString() should be ("[many|+displaced]")
  }
  
  "rainfallPovertyEdge" should "have a string representation" in {
    !rainfallPovertyEdge.toString().isEmpty()
  }
}
