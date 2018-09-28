package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.mentions.EidosEventMention
import org.clulab.wm.eidos.test.TestUtils._

class TestNegation extends Test {
  it should "find no negation when not negated" in {
    val text1 = "Rainfall causes flooding."
    val mentions1 = extractEidosMentions(text1)
    mentions1.filter(eem => eem.isInstanceOf[EidosEventMention])
      .exists(m => m.asInstanceOf[EidosEventMention].negations.nonEmpty) should be (false)
  }

  it should "find no negation when there is a single negation" in {
    val text1 = "Rainfall does not cause flooding."
    val mentions1 = extractEidosMentions(text1)
    mentions1.filter(eem => eem.isInstanceOf[EidosEventMention])
      .exists(m => m.asInstanceOf[EidosEventMention].negations.size == 1) should be (true)
  }

  it should "find no negation when double negated" in {
    val text1 = "Rainfall does not not cause flooding."
    val mentions1 = extractEidosMentions(text1)
    mentions1.filter(eem => eem.isInstanceOf[EidosEventMention])
      .exists(m => m.asInstanceOf[EidosEventMention].negations.nonEmpty) should be (false)
  }

  it should "find negation when ridiculously (and of odd cardinality) negated" in {
    val text1 = "Rainfall does not not not cause flooding."
    val mentions1 = extractEidosMentions(text1)
    mentions1.filter(eem => eem.isInstanceOf[EidosEventMention])
      .exists(m => m.asInstanceOf[EidosEventMention].negations.size == 1) should be (true)
  }
}
