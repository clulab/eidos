package org.clulab.wm.eidos.text

import org.clulab.odin.EventMention
import org.clulab.wm.eidos.test.TestUtils._

class TestUnicode extends Test {

  // Some unicode characters are parsed unfavorably for Eidos and rules have been written to
  // deal them.  This tests that the rules are really working and should be expanded as more
  // problematic cases are discovered.

  {
    val text = "\u2022 The wetter than average conditions in mid May, led to average to above average " +
        "vegetation developing along the west, central and north parts of South Sudan;"
    val annotatedDocument = ieSystem.extractFromText(text)
    // This can't be run the normal way because of a unicode check.
    val odinMentions = annotatedDocument.odinMentions
    val eidosMentions = annotatedDocument.eidosMentions

    behavior of "text with bullet"

    it should "not be disturbed by the bullet" in {
      odinMentions.exists(_.text == "\u2022 The wetter") should be (false)

      odinMentions.exists(_.text == "conditions in mid May") should be (true)
      eidosMentions.exists(_.canonicalName == "conditions in mid May") should be (true)

//      odinMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.text) }
//      eidosMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.canonicalName) }

//      eidosMentions.exists(_.canonicalName.contains("\u2022")) should be (false)
        eidosMentions.exists { eidosMention =>
          eidosMention.odinMention.isInstanceOf[EventMention] &&
              eidosMention.canonicalName.contains("\u2022")
        } should be (false)
    }
  }

  {
    val text = "\u27a4\u27a4 Across Central Asia, reduced precipitation leading to below-average snow accumulation is most likely across west " +
        "central Afghanistan and Tajikistan during the 2017-2018 winter growing season."
    val annotatedDocument = ieSystem.extractFromText(text)
    // This can't be run the normal way because of a unicode check.
    val odinMentions = annotatedDocument.odinMentions
    val eidosMentions = annotatedDocument.eidosMentions

    behavior of "text with arrows"

    it should "not be disturbed by the arrows" in {
      odinMentions.exists(_ == "\u27a4\u27a4 Across Central Asia") should be (false)

      odinMentions.exists(_.text == "snow accumulation") should be (true)
      eidosMentions.exists(_.canonicalName == "snow accumulation") should be (true)

//      odinMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.text) }
//      eidosMentions.foreach { mention => println(mention.getClass().getName() + ": " + mention.canonicalName) }

//      eidosMentions.exists(_.canonicalName.contains("\u27a4")) should be (false)
      eidosMentions.exists { eidosMention =>
        eidosMention.odinMention.isInstanceOf[EventMention] &&
            eidosMention.canonicalName.contains("\u27a4")
      } should be (false)
    }
  }
}
