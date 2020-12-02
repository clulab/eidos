package org.clulab.wm.eidos.text.english.cag

import org.clulab.wm.eidos.test.ExtractionTest
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.clulab.wm.eidoscommon.utils.Timer

class TestCags extends ExtractionTest {

  behavior of "long texts"

  it should "finish quickly" taggedAs(Keith) in {
    val stringBuilder = new StringBuilder(fullText + (" " + fullText) * 4) // + fullText // Because starting at 2

    // Pump the system once to get lazy loading
    ieSystem.annotate(stringBuilder.toString())

    for (i <- 5 to 5 by 5) {
      val text = stringBuilder.toString()

      Timer.time(s"Run ${i} is starting with this text of length ${text.size}.") {
        extractMentions(text)
      }
      stringBuilder.append((" " + fullText) * 5)
    }
  }
}
