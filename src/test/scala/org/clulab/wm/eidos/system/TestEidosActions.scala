package org.clulab.wm.eidos.system

import java.util.IdentityHashMap

import scala.collection.JavaConverters._
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.{Document, Sentence}
import org.clulab.serialization.json.stringify
import org.clulab.struct.Interval
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.MentionUtils

class TestEidosActions extends ExtractionTest {

  def addAllMentions(mentions: Seq[Mention], mapOfMentions: IdentityHashMap[Mention, Mention]): Unit = {
    def mentionsInPaths(paths: Map[String, Map[Mention, SynPath]]): Seq[Mention] =
      Seq.empty
    //        paths.values.map(_.keys).flatten.toSeq

    mentions.foreach { mention =>
      mapOfMentions.put(mention, mention)

      val arguments = mention.arguments.values.flatten.toSeq

      mention match {
        case mention: TextBoundMention => addAllMentions(arguments, mapOfMentions)
        // One trigger can result in multiple events, so triggers should not be involved in the search for duplicates.
        // Generally the two trigger instances would have different rules, but even if they were the same, as long as
        // the resulting EventMentions are different, duplicate triggers are not a problem.
        case mention: EventMention => addAllMentions(arguments ++ mentionsInPaths(mention.paths) /* :+ mention.trigger */, mapOfMentions)
        case mention: RelationMention => addAllMentions(arguments ++ mentionsInPaths(mention.paths), mapOfMentions)
      }
    }
  }

  def findUniqueMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val mapOfMentions = new IdentityHashMap[Mention, Mention]()

    addAllMentions(mentions, mapOfMentions)
    mapOfMentions.keySet.asScala.toSeq
  }

  def areMatching(left: Mention, right: Mention): Boolean = {
    if (left.eq(right))
      true
    else {
      left.getClass == right.getClass &&
        left.document.eq(right.document) &&
        left.sentence == right.sentence &&
        left.labels == right.labels &&
        left.tokenInterval == right.tokenInterval &&
        left.arguments.keys == right.arguments.keys && {
        // Get the key for which there is a mismatch
        val mismatch = left.arguments.keys.find { key =>
          val leftValues = left.arguments(key)
          val rightValues = right.arguments(key)

          if (leftValues.size != rightValues.size) true
          // Expecting the same order is a little much
          else leftValues.zip(rightValues).exists(pair => !areMatching(pair._1, pair._2))
        }
        mismatch.isEmpty
      }
    }
  }

  def findMatchingPair(mention: Mention, mentions: Seq[Mention]): Option[(Mention, Mention)] =
    mentions match {
      case Seq() => None
      case Seq(head) => if (areMatching(mention, head)) Some((mention, head)) else None
      case Seq(head, tail @ _*) => if (areMatching(mention, head)) Some((mention, head)) else findMatchingPair(mention, tail)
    }

  // Note that it is possible for this to recurse forever.
  // However, it is unlikely and this is only a test.
  def findMatchingPair(mentions: Seq[Mention]): Option[(Mention, Mention)] =
    mentions match {
      case Seq() => None
      case Seq(head) => None
      case Seq(head, tail @ _*) => {
        val matchingPair = findMatchingPair(head, tail)

        if (matchingPair.isDefined) matchingPair
        else findMatchingPair(tail)
      }
    }

  protected def test(reader: EidosSystem, text: String, index: Int): Unit = {
    val annotatedDocument = reader.extractFromText(text)
    val someMentions = annotatedDocument.odinMentions
    val uniqueMentions = findUniqueMentions(someMentions)
    val matchingPair = findMatchingPair(uniqueMentions)

    if (matchingPair.isDefined) {
      val jValueLeft = WMJSONSerializer.jsonAST(Seq(matchingPair.get._1))
      val jValueRight = WMJSONSerializer.jsonAST(Seq(matchingPair.get._2))
      val jsonLeft = stringify(jValueLeft, pretty = true)
      val jsonRight = stringify(jValueRight, pretty = true)

      println(jsonLeft)
      println(jsonRight)

      val jValueAll = WMJSONSerializer.jsonAST(someMentions)
      val jsonAll = stringify(jValueAll, pretty = true)
      println(jsonAll)
    }
    it should "produce not just unique but also distinct mentions " + index in {
      matchingPair should be (None)
    }
  }

  def correctCoreference(m: Mention, antecedant: String, anaphor: String): Boolean = {
    (m matches EidosSystem.COREF_LABEL) && (m.isInstanceOf[CrossSentenceMention]) &&
      (m.arguments(EidosActions.ANTECEDENT).head.text == antecedant) &&
      (m.arguments(EidosActions.ANAPHOR).head.text == anaphor)
  }


  def hasProperty(m: Mention, term: String): Boolean = {
    val props = m.attachments.filter(_.isInstanceOf[Property])
    props.exists(p => p.asInstanceOf[Property].trigger == term)
  }


//  {
//    val reader = new EidosSystem()
//
//    behavior of "EidosActions"
//
//    val texts = Seq(
//      // This is something that Ben found.
//      "The government promotes improved cultivar to boost agricultural production for ensuring food security.",
//      // This one is sent19 in the TestRaps
//      "The governmental policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector, " +
//        "and increased export earnings through increased productivity, efficient input use, and better market access, " +
//        "infrastructure, and service development.",
//      "This is a test."
//    )
//
//    texts.zipWithIndex.foreach { case (text, index) => test(reader, text, index) }
//  }

  class TestEidosActions extends EidosActions(new Taxonomy(Map.empty)) {
    // Relax some protected functions for testing

    override def filterSubstringTriggers(attachments: Seq[TriggeredAttachment]): Seq[TriggeredAttachment] =
      super.filterSubstringTriggers(attachments)

    override def filterMostComplete(attachments: Seq[TriggeredAttachment]): TriggeredAttachment =
      super.filterMostComplete(attachments)
  }

  val eidosActions = new TestEidosActions()

  behavior of "attachment merging"

  it should "filter for duplicate strings" in {
    val attachments = Seq(
      Quantification("long trigger", None),
      Quantification("long trigger", None)
    )
    val filtered = eidosActions.filterSubstringTriggers(attachments)

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
    val filtered = eidosActions.filterSubstringTriggers(attachments)

    filtered.size should be (2)
  }

  it should "filter for completeness" in {
    val attachments = Seq(
      Quantification("trigger", None),
      Quantification("long trigger", Some(Seq("one", "two"))),
      Quantification("long trigger", Some(Seq("one", "two", "three"))),
      Quantification("short trigger", Some(Seq.empty))
    )
    val filtered = eidosActions.filterMostComplete(attachments)

    filtered.asInstanceOf[EidosAttachment].argumentSize should be (3)
  }

  it should "filter homogeneous attachments for both including type and trigger" in {
    val attachments = Seq(
      Quantification("trigger", None),
      Quantification("long trigger", Some(Seq("one", "two"))),
      Quantification("long trigger", Some(Seq("one", "two", "three"))),
      Quantification("short trigger", Some(Seq.empty))
    )
    val filtered = eidosActions.filterAttachments(attachments)

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

    val filtered = eidosActions.filterAttachments(attachments)

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
    val afterMentions = eidosActions.mergeAttachments(beforeMentions, new State())

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
    val afterMentions = eidosActions.mergeAttachments(beforeMentions, new State())

    afterMentions.size should be (1)
    afterMentions.head.attachments.size should be(3)
  }

  it should "not throw an Exception" in {
    val text = "Accordingly , the Cooperative Union has agreed to process locally produced maize and haricot bean as a blended food that meets the standards set by the Bureau of Education ."

    noException should be thrownBy
      extractMentions(text)
  }

  it should "identify simple coreference relations in adjacent sentences" in {
    val text = "Rainfall causes flooding.  This causes problems."
    val mentions = extractMentions(text)
    mentions.exists(m => correctCoreference(m, "flooding", "This"))
  }

  it should "identify properties" in {
    val text = "Rainfall increases the price of puppies."
    val mentions = extractMentions(text)
    mentions.exists(m => hasProperty(m, "price")) should be (true)
  }
}
