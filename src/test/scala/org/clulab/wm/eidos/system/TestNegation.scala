package org.clulab.wm.eidos.system

import org.clulab.odin.EventMention
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.MentionUtils.hasNegation


class TestNegation extends ExtractionTest {

  it should "find no negation when not negated" in {
    val text1 = "Rainfall causes flooding."
    val mentions1 = extractMentions(text1)
    mentions1.filter(em => em.isInstanceOf[EventMention]).exists(hasNegation) should be (false)
  }

  it should "find negation when there is a single negation" in {
    val text1 = "Rainfall does not cause flooding."
    val mentions1 = extractMentions(text1)
    mentions1.filter(em => em.isInstanceOf[EventMention]).exists(hasNegation) should be (true)
  }

  it should "find no negation when double negated" in {
    val text1 = "Rainfall does not not cause flooding."
    val mentions1 = extractMentions(text1)
    mentions1.filter(em => em.isInstanceOf[EventMention]).exists(hasNegation) should be (false)
  }

  it should "find negation when ridiculously (and of odd cardinality) negated" in {
    val text1 = "Rainfall does not not not cause flooding."
    val mentions1 = extractMentions(text1)
    mentions1.filter(em => em.isInstanceOf[EventMention]).exists(hasNegation) should be (true)
  }

  it should "not match negation contained in the event arguments" in {
    val text1 = "Many families do not have food because of the increase in conflict."
    val mentions1 = extractMentions(text1)
    mentions1.filter(em => em.isInstanceOf[EventMention]).exists(hasNegation) should be (false)
  }
}
