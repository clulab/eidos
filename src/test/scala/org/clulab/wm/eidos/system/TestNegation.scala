package org.clulab.wm.eidos.system

import org.clulab.odin.{EventMention, Mention}
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.test.TestUtils._


class TestNegation extends Test {

  def hasNegation(m: Mention): Boolean = m.attachments.exists(_.isInstanceOf[Negation])

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
}
