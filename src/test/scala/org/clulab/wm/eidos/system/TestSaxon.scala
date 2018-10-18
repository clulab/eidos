package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

/**
 * This is an attempt to test the issue "DOMSource cannot be processed #160"
 * that is documented at https://github.com/clulab/eidos/issues/160.
 * Unfortunately, the problem is not reproduced here.  The patch to
 * build.sbt will be accepted anyway.
 */
class TestSaxon extends EnglishTest {
  
  {
    val s1 = "After Nov. 6, 2006, anxiety increased.  The month of November was the cause. " +
        "Dates like November and years like 2017 seem to cause problems. " +
        "They are not apparent here. "
    val tester = new GraphTester(s1)
    
    behavior of "a sentence with a date"
    
    it should "not provoke complaints" in {
      tester.test(AntiNodeSpec("hello"))
    }
  }
}
