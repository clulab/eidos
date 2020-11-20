package org.clulab.wm.eidos.system

import org.clulab.odin.TextBoundMention
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.{AttachmentHandler, Decrease, EidosAttachment, Increase, Quantification}
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.utils.MentionUtils

class TestAttachmentHandler extends ExtractionTest {

  val attachmentHandler = new AttachmentHandler()

  it should "merge attachments" in {
    val attachments = Seq(
      Increase("abc", None),
      Increase("abcd", Some(Seq("one", "two"))),
      Increase("xyz", Some(Seq("one", "two", "three"))),
      Increase("xyz", Some(Seq.empty)),

      Decrease("31415", None),
      Decrease("314", Some(Seq("one", "two"))),
      Decrease("314", Some(Seq("one", "two", "three"))),
      Decrease("31415", Some(Seq("pi")))
    )

    val beforeMentions = Seq(new TextBoundMention(Seq("Entity"), Interval(2, 3), 5, new Document(Array.empty[Sentence]), false, "Found by me", attachments.toSet))
    val afterMentions = attachmentHandler.mergeAttachments(beforeMentions)

    afterMentions.head.attachments.size should be(3)
  }

  it should "merge entities" in {
    val attachments1 = Seq(
      Increase("abc", None),
      Increase("abcd", Some(Seq("one", "two"))),
      Increase("xyz", Some(Seq("one", "two", "three"))),
      Increase("xyz", Some(Seq.empty))
    )
    val attachments2 = Seq(
      Decrease("31415", None),
      Decrease("314", Some(Seq("one", "two"))),
      Decrease("314", Some(Seq("one", "two", "three"))),
      Decrease("31415", Some(Seq("pi")))
    )

    val beforeMentions = Seq(
      new TextBoundMention(Seq("Entity"), Interval(2, 3), 5, new Document(Array.empty[Sentence]), false, "Found by me", attachments1.toSet),
      new TextBoundMention(Seq("Entity"), Interval(2, 3), 5, new Document(Array.empty[Sentence]), false, "Found by me", attachments2.toSet)
    )
    val afterMentions = attachmentHandler.mergeAttachments(beforeMentions)

    afterMentions.size should be (1)
    afterMentions.head.attachments.size should be(3)
  }

  behavior of "attachment merging"

  it should "filter for duplicate strings" in {
    val attachments = Seq(
      Quantification("long trigger", None),
      Quantification("long trigger", None)
    )
    val filtered = AttachmentHandler.filterSubstringTriggers(attachments)

    filtered.size should be (1)
  }

  it should "filter for substrings" in {
    val attachments = Seq(
      Quantification("trigger", None),
      Quantification("long trigger", None),
      Quantification("long trigger", None),
      Quantification("ng tr", None),
      Quantification("long", None),
      Quantification("short", None)
    )
    val filtered = AttachmentHandler.filterSubstringTriggers(attachments)

    filtered.size should be (2)
  }

  it should "filter for completeness" in {
    val attachments = Seq(
      Quantification("trigger", None),
      Quantification("long trigger", Some(Seq("one", "two"))),
      Quantification("long trigger", Some(Seq("one", "two", "three"))),
      Quantification("short trigger", Some(Seq.empty))
    )
    val filtered = AttachmentHandler.filterMostComplete(attachments)

    filtered.asInstanceOf[EidosAttachment].argumentSize should be (3)
  }

  it should "filter homogeneous attachments for both including type and trigger" in {
    val attachments = Seq(
      Quantification("trigger", None),
      Quantification("long trigger", Some(Seq("one", "two"))),
      Quantification("long trigger", Some(Seq("one", "two", "three"))),
      Quantification("short trigger", Some(Seq.empty))
    )
    val filtered = AttachmentHandler.filterTriggeredAttachments(attachments)

    filtered.size should be(2)
    filtered.foreach { attachment =>
      if (attachment.trigger == "short trigger")
        attachment.asInstanceOf[EidosAttachment].argumentSize should be(0)
      if (attachment.trigger == "long trigger")
        attachment.asInstanceOf[EidosAttachment].argumentSize should be(3)
    }
  }

  it should "filter heterogeneous attachments for both including type and trigger" in {
    val attachments = Seq(
      Increase("abc", None),
      Increase("abcd", Some(Seq("one", "two"))),
      Increase("xyz", Some(Seq("one", "two", "three"))),
      Increase("xyz", Some(Seq.empty)),

      Decrease("31415", None),
      Decrease("314", Some(Seq("one", "two"))),
      Decrease("314", Some(Seq("one", "two", "three"))),
      Decrease("31415", Some(Seq("pi")))
    )

    val filtered = AttachmentHandler.filterTriggeredAttachments(attachments)

    filtered.size should be (3)
  }

  it should "copy with attachments" in {
    val attachments = Seq(
      Increase("abc", None),
      Increase("abcd", Some(Seq("one", "two"))),
      Increase("xyz", Some(Seq("one", "two", "three"))),
      Increase("xyz", Some(Seq.empty)),

      Decrease("31415", None),
      Decrease("314", Some(Seq("one", "two"))),
      Decrease("314", Some(Seq("one", "two", "three"))),
      Decrease("31415", Some(Seq("pi")))
    )

    val mention = new TextBoundMention(Seq("label"), Interval(2, 3), 5, Document(Array.empty[Sentence]), false, "Found by me", Set.empty)
    mention.attachments.size should be(0)

    val newMention = MentionUtils.withMoreAttachments(mention, attachments)
    newMention.attachments.size should be(attachments.size)
  }
}
