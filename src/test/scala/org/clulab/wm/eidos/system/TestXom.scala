package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.AntiNodeSpec

class TestXom extends Test {
  
  {
    val s1 = "Nov. 6, 2006 caused an increase in anxiety."
    val tester = new Tester(s1)
    
    behavior of "a sentence with a date"
    
    it should "not provoke complaints" in {
      tester.test(AntiNodeSpec("hello"))
    }
  }
}
