package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.ExtractionTest
import org.clulab.wm.eidoscommon.EidosParameters


class TestFiltering extends ExtractionTest {

  it should "not find entities that are a single character" in {
    val text1 = "The % causes flooding."
    val mentions1 = extractMentions(text1)
    val causal = mentions1.filter(_ matches EidosParameters.CAUSAL_LABEL)
    causal should have size(1)
    val valid = ieSystem.components.stopwordManagerOpt.get.keepCAGRelevant(causal)
    valid should be(empty)
  }

  it should "not find entities that are a single letter" in {
    val text1 = "The R causes flooding."
    val mentions1 = extractMentions(text1)
    val causal = mentions1.filter(_ matches EidosParameters.CAUSAL_LABEL)
    causal should have size(1)
    val valid = ieSystem.components.stopwordManagerOpt.get.keepCAGRelevant(causal)
    valid should be(empty)
  }

  it should "not find entities that have numbers and stops only" in {
    val text1 = "The 50% causes flooding."
    val mentions1 = extractMentions(text1)
    val causal = mentions1.filter(_ matches EidosParameters.CAUSAL_LABEL)
    causal should have size(1)
    val valid = ieSystem.components.stopwordManagerOpt.get.keepCAGRelevant(causal)
    valid should be(empty)
  }
}
