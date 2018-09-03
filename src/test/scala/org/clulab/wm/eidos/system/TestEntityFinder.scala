package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._

class TestEntityFinder extends Test {

  "rainfall entity" should "have leading preposition trimmed" in {
    val text = "The decrease in rainfall was a problem."
    val mentions = extractMentions(text)
    mentions.filter(_.text == "rainfall") should have size (1)
    mentions.filter(_.text == "in rainfall") should have size (0)
  }

  "JJ entity" should "only be kept if following word was avoided as a trigger" in {
    // Note -- this is a fake verb bc to test what the entityfinder is returning we don't want to trigger
    // the argument expansion
    val text1 = "some economic blarged the recession."
    val mentions1 = extractMentions(text1)
    mentions1.exists(_.text == "economic") should be (false)

    // Note -- this is a fake verb bc to test what the entityfinder is returning we don't want to trigger
    // the argument expansion
    val text2 = "some economic declines blarged the recession."
    val mentions2 = extractMentions(text2)
    mentions2.exists(_.text == "economic") should be (true)
  }


}
