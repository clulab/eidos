package org.clulab.wm.eidos.text

import org.clulab.wm.eidos.test.TestUtils._

class TestUnicode extends ExtractionTest {

  // Some unicode characters are parsed unfavorably for Eidos and rules have been written to
  // deal them.  This tests that the rules are really working and should be expanded as more
  // problematic cases are discovered.

  // The original test sentences no longer result in any events found.  They have been replaced
  // by made up sentences that might be replaced if ever we come across a problem case.

  {
    val text = "\u2022 " + "Poverty caused significantly increased hunger."
    // This can't be run the normal test way because of a unicode check.
    val annotatedDocument = ieSystem.extractFromText(text)
    val odinMentions = annotatedDocument.odinMentions
    val eidosMentions = annotatedDocument.eidosMentions

    behavior of "text with bullet"

    ignore should "not be disturbed by the bullet" in {
      odinMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.text) }
      eidosMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.canonicalName) }

      odinMentions.exists(_.text == "Poverty") should be (true)
      odinMentions.exists(_.text == "\u2022 Poverty") should be (false)

      eidosMentions.exists(_.canonicalName == "poverty") should be (true)
      eidosMentions.exists(_.canonicalName.contains("\u2022")) should be (false)
    }
  }

  {
    val text = "\u27a4\u27a4 " + "Poverty caused significantly increased hunger."
    // This can't be run the normal way because of a unicode check.
    val annotatedDocument = ieSystem.extractFromText(text)
    val odinMentions = annotatedDocument.odinMentions
    val eidosMentions = annotatedDocument.eidosMentions

    behavior of "text with arrows"

    ignore should "not be disturbed by the arrows" in {
      odinMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.text) }
      eidosMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.canonicalName) }

      odinMentions.exists(_.text == "Poverty") should be (true)
      odinMentions.exists(_ == "\u27a4\u27a4 Poverty") should be (false)

      eidosMentions.exists(_.canonicalName == "poverty") should be (true)
      eidosMentions.exists(_.canonicalName.contains("\u27a4")) should be (false)
    }
  }
}
