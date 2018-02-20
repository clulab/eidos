package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.scalactic.source.Position.apply

class TestEntityFinder extends Test {

  "rainfall entity" should "have leading preposition trimmed" in {
    val text = "The decrease in rainfall was a problem."
    val mentions = extractMentions(text)
    mentions.filter(_.text == "rainfall") should have size (1)
    mentions.filter(_.text == "in rainfall") should have size (0)
  }

}
