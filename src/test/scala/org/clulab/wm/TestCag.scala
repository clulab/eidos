package org.clulab.wm

import CAG._
import TestUtils._

class TestCag extends Test {
  def getSpecialCharsSize(s: String) = s.filter(c => c > 127).size
  
  "p1" should "have the correct number of special characters" in {
    getSpecialCharsSize(p1) should be (2)
  }
  
  "p2" should "have the correct number of special characters" in {
    getSpecialCharsSize(p2) should be (1)
  }
  it should "have the correct number of double spaces" in {
    CAG.DOUBLE_SENTENCE_SEPARATOR.r.findAllMatchIn(p2).length should be (3)
  }
  
  "p3" should "have the correct number of special characters" in {
    getSpecialCharsSize(p3) should be (0)
  }

  "p4" should "have the correct number of special characters" in {
    getSpecialCharsSize(p4) should be (1)
  }

  "p5" should "have the correct number of special characters" in {
    getSpecialCharsSize(p5) should be (0)
  }
  
  "p6" should "have the correct number of special characters" in {
    getSpecialCharsSize(p6) should be (0)
  }
}
