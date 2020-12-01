package org.clulab.wm.eidos.utils

import org.clulab.odin.Mention
import org.clulab.wm.eidos.mentions.MentionUtils
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidoscommon.EidosParameters

class TestMentionUtils extends ExtractionTest {

  it should "find (pseudo)trigger info from EventMention and its arguments" in {
    val text = "Heavy seasonal rainfall causes severe flooding."
    val causal = extractMentions(text).filter(_ matches EidosParameters.CAUSAL_LABEL)
    causal should have size(1)

    val causalTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(causal.head)
    causalTriggerInfo.text should be("causes")
    causalTriggerInfo.start should be(24)
    causalTriggerInfo.end should be(30)

    val cause = causal.head.arguments("cause")
    cause should have size(1)
    val causeTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(cause.head)
    causeTriggerInfo.text should be("rainfall")
    causeTriggerInfo.start should be(15)
    causeTriggerInfo.end should be(23)

    val effect = causal.head.arguments("effect")
    effect should have size(1)
    val effectTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(effect.head)
    effectTriggerInfo.text should be("flooding")
    effectTriggerInfo.start should be(38)
    effectTriggerInfo.end should be(46)
  }

  def testTriggerInfo(text: String, mentions: Seq[Mention], trigger: String): Unit = {
    mentions should have size (1)

    val mention = mentions.head
    val triggerInfo = MentionUtils.synHeadOfMentionOrTrigger(mention)

    triggerInfo.text should be(trigger)
    triggerInfo.start should be(text.indexOf(trigger))
    triggerInfo.end should be(text.indexOf(trigger) + trigger.length)
  }

  it should "handle punct characters and spacing in the TriggerInfo" in {
    // Raw text will have "; word, ``.  This can change offsets.
    // Raw text will have n-dash; word, plain dash.  Output the text, not the word.
    val text = "The locality\"s rain\u2013fall, in July, causes severe flooding."
    val causal = extractMentions(text).filter(_ matches EidosParameters.CAUSAL_LABEL)

    testTriggerInfo(text, causal, "causes")
    testTriggerInfo(text, causal.head.arguments("cause"), "rain\u2013fall")
    testTriggerInfo(text, causal.head.arguments("effect"), "flooding")
  }
}
