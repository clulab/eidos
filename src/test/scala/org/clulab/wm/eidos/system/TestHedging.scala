package org.clulab.wm.eidos.system

import org.clulab.odin.Mention
import org.clulab.wm.eidos.attachments.Hedging
import org.clulab.wm.eidos.test.TestUtils._


class TestHedging extends ExtractionTest {

  def hasHedging(m: Mention, term: String): Boolean= {
    val hedged = m.attachments.filter(_.isInstanceOf[Hedging])
    hedged.exists(h => h.asInstanceOf[Hedging].trigger == term)
  }

  it should "identify hedging" in {
    val text = "Rainfall is likely to cause flooding."
    val mentions = extractMentions(text)
    mentions.exists(m => hasHedging(m, "likely")) should be (true)
  }

}