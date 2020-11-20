package org.clulab.wm.eidos.system

import org.clulab.odin.{CrossSentenceMention, Mention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidoscommon.EidosParameters

class TestCorefHandler extends ExtractionTest {

  def correctCoreference(m: Mention, antecedant: String, anaphor: String): Boolean = {
    (m matches EidosParameters.COREF_LABEL) && (m.isInstanceOf[CrossSentenceMention]) &&
      (m.arguments(CorefHandler.ANTECEDENT).head.text == antecedant) &&
      (m.arguments(CorefHandler.ANAPHOR).head.text == anaphor)
  }

  it should "identify simple coreference relations in adjacent sentences" in {
    val text = "Rainfall causes flooding.  This causes problems."
    val mentions = extractMentions(text)
    mentions.exists(m => correctCoreference(m, "flooding", "This"))
  }
}
