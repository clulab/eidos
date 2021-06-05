package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.test.EnglishGroundingTest


class TestSRLGrounder extends EnglishGroundingTest {

  behavior of "SRLCompositionalGrounder"

  it should "not recurse infinitely when there are two properties that point at each other" in {
    val text = "BOOST INCOME TO SUPPORT FOOD SECURITY AND NUTRITION ."
    val mentions = ieSystem.extractFromText(text).eidosMentions
    mentions.size should be (2)
    val groundings  = mentions.map(_.grounding("wm_compositional").grounding).map(_.head.name)
    groundings.exists(gr => gr.contains("THEME: wm/concept/health/nutrition/")) shouldBe (true)
    groundings.exists(gr =>
      gr.contains("THEME: wm/property/security") &&
      gr.contains("Theme properties: wm/property/support")
    ) shouldBe (true)
  }

}
