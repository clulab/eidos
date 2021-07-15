package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.wm.eidos.test.EnglishGroundingTest
import org.clulab.wm.eidoscommon.utils.FileUtils

class TestSRLGrounder extends EnglishGroundingTest {

  behavior of "SRLCompositionalGrounder"

  it should "not recurse infinitely when there are two properties that point at each other" in {
    val text = "BOOST INCOME TO SUPPORT FOOD SECURITY AND NUTRITION ."
    val annotatedDocument = ieSystem.extractFromText(text)
    val mentions = annotatedDocument.eidosMentions
    mentions.size should be (2)
    // The tested grounding was no longer at the head, so they are all collected.
    val groundings  = mentions.map(_.grounding("wm_compositional").grounding).flatMap {
      multipleOntologyGrounding => multipleOntologyGrounding.map(_.name)
    }
    groundings.exists(gr => gr.contains("THEME: wm/concept/health/nutrition/")) shouldBe (true)
    groundings.exists(gr =>
      gr.contains("THEME: wm/concept/goods/food") &&
      gr.contains("Theme properties: wm/property/security") &&
      gr.contains("THEME PROCESS: wm/property/support")
    ) shouldBe (true)
  }

  it should "not overflow the stack when there is a loop in the semantic roles" in {
    val text = FileUtils.getTextFromResource("/data/srl_loop.txt")
    val annotatedDocument = ieSystem.extractFromText(text)
    val mentions = annotatedDocument.eidosMentions
    // The tested grounding was no longer at the head, so they are all collected.
    val groundings  = mentions.map(_.grounding("wm_compositional").grounding).flatMap {
      multipleOntologyGrounding => multipleOntologyGrounding.map(_.name)
    }
    groundings.toArray shouldNot be(empty)
    groundings.exists(gr => gr != "Empty Compositional Grounding") should be (true)
  }
}
