package org.clulab.wm.eidos.test

import org.clulab.wm.eidos.test.TestUtils._

class TestTest extends ExtractionTest {
  
  behavior of "Test"
  
  it should "not be ignored" in {
  }
  
  ignore should "be ignored" taggedAs(Becky) in {
    6 should be (5)
  }
  
  ignore should "still be ignored" taggedAs(Becky, Keith) in {
    5 should be (6)
  }
  
  passingTest should "pass" in {
    1 should be (1)
  }
  
  failingTest should "fail" in {
    1 should be (2)
  }
  
  ignore should "compile" in {
    5 shouldBe (successful)
    5 should be (successful)
    5 should be(successful)
    // 5 should be successful // Doesn't work
  }
  
  it should "clean strings properly" in {
    val tester = new GraphTester("")
    
    tester.clean("  line one\n\nline two \n line three\n\n ") should be ("line one line two line three")

    an [IllegalArgumentException] should be thrownBy
        tester.clean(" \u2013 ")
  }
}