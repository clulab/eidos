package org.clulab.wm

import org.scalatest._
import CAG._

class TestCag extends FlatSpec with Matchers {
  def getSpecialCharsSize(s: String) = s.filter(c => c > 127).size
  
  "p1" should "be reasonable" in {
    getSpecialCharsSize(p1) should be (2)
  }
  
  "p2" should "be reasonable" in {
    getSpecialCharsSize(p2) should be (1)
  }
  
  "p3" should "be reasonable" in {
    getSpecialCharsSize(p3) should be (0)
  }

  "p4" should "be reasonable" in {
    getSpecialCharsSize(p4) should be (1)
  }

  "p5" should "be reasonable" in {
    getSpecialCharsSize(p5) should be (0)
  }
  
  "p6" should "be reasonable" in {
    getSpecialCharsSize(p6) should be (0)
  }
}
