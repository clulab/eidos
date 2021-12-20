package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.EnglishTest

class TestArgumentExpansion extends EnglishTest {

  behavior of "ArgumentExpander"

  it should "not recurse infinitely" in {
    val text = "15.0 20.0 25.0 30.0 35.0 40.0 45.0 Ja n -1 7 A p r1 7 Ju l1 7 O ct -1 7 Ja n -1 8 A p r1 8 Ju l1 8 O ct -1 8 Ja n -1 9 A p r1 9 Ju l1 9 O ct -1 9 Ja n -2 0 A p r2 0 Ju l2 0 Price trend of teffin Addis Ababa market Teff' white Teff' mixed Maize prices reduced or remained stable since June through August in most markets following harvests in unimodal areas but also increased imports from Uganda and Tanzania after the Government issued permits for private companies to import about 4 million bags of maize."
    val mentions = extractMentions(text)
    mentions.length should be (0)
  }


}
