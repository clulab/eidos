package org.clulab.wm.eidos.text.cag

import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.cag.CAG._
import org.clulab.wm.eidos.utils.Timer

class TestCags extends Test {

  behavior of "long texts"

  it should "finish quickly" taggedAs(Keith) in {
    val stringBuilder = new StringBuilder(fullText + (" " + fullText) * 4) // + fullText // Because starting at 2

    // Pump the system once to get lazy loading
    TestUtils.ieSystem.annotate(stringBuilder.toString(), keepText = true)

    for (i <- 5 to 5 by 5) {
      val text = stringBuilder.toString()

      Timer.time(s"Run ${i} is starting with this text of length ${text.size}.") {
        TestUtils.extractMentions(text)
      }
      stringBuilder.append((" " + fullText) * 5)
    }
  }
}
