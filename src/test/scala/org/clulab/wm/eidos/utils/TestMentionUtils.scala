package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest

class TestMentionUtils extends ExtractionTest{
  it should "find (pseudo)trigger info from EventMention and its arguments" in {
    val text1 = "Heavy seasonal rainfall causes severe flooding."
    val causal = extractMentions(text1).filter(_ matches EidosSystem.CAUSAL_LABEL)
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

  it should "handle punct characters and spacing in the TriggerInfo" in {
    val text1 = "The locality's rain-fall, in July, causes severe flooding."
    val causal = extractMentions(text1).filter(_ matches EidosSystem.CAUSAL_LABEL)
    causal should have size(1)

    val causalTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(causal.head)
    causalTriggerInfo.text should be("causes")
    causalTriggerInfo.start should be(35)
    causalTriggerInfo.end should be(41)

    val cause = causal.head.arguments("cause")
    cause should have size(1)
    val causeTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(cause.head)
    causeTriggerInfo.text should be("rain-fall")
    causeTriggerInfo.start should be(15)
    causeTriggerInfo.end should be(24)

    val effect = causal.head.arguments("effect")
    effect should have size(1)
    val effectTriggerInfo = MentionUtils.synHeadOfMentionOrTrigger(effect.head)
    effectTriggerInfo.text should be("flooding")
    effectTriggerInfo.start should be(49)
    effectTriggerInfo.end should be(57)
  }

}
