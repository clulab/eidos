package org.clulab.wm.eidos.system

import java.util.IdentityHashMap

import org.clulab.odin._
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.attachments.Property
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.actions.EidosActions

import scala.collection.JavaConverters._

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


  def hasProperty(m: Mention, term: String): Boolean = {
    m.attachments.collect{ case p: Property => p.trigger }.contains(term)
  }

  val eidosActions = new EidosActions(None)

  it should "not throw an Exception" in {
    val text = "Accordingly , the Cooperative Union has agreed to process locally produced maize and haricot bean as a blended food that meets the standards set by the Bureau of Education ."

    noException should be thrownBy
      extractMentions(text)
  }

  it should "identify properties" in {
    val text = "Rainfall increases the price of puppies."
    val mentions = extractMentions(text)
    mentions.exists(m => hasProperty(m, "price")) should be (true)
  }
}
