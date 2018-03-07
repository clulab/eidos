package org.clulab.wm.eidos.system

import java.util.IdentityHashMap

import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.SynPath
import org.clulab.odin.TextBoundMention
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.JavaConverters.asScalaSet

class TestEidosActions extends Test {
  
  def addAllMentions(mentions: Seq[Mention], mapOfMentions: IdentityHashMap[Mention, Mention]): Unit = {
    def mentionsInPaths(paths: Map[String, Map[Mention, SynPath]]): Seq[Mention] =
        Seq.empty
//        paths.values.map(_.keys).flatten.toSeq
    
    mentions.foreach { mention =>
      mapOfMentions.put(mention, mention)
      
      val arguments = mention.arguments.values.flatten.toSeq
       
      mention match {
        case mention: TextBoundMention => addAllMentions(arguments, mapOfMentions)
        case mention: EventMention => addAllMentions(arguments ++ mentionsInPaths(mention.paths) :+ mention.trigger, mapOfMentions)
        case mention: RelationMention => addAllMentions(arguments ++ mentionsInPaths(mention.paths), mapOfMentions)
      }
    }
  }
    
  def findUniqueMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val mapOfMentions = new IdentityHashMap[Mention, Mention]()
    
    addAllMentions(mentions, mapOfMentions)
    asScalaSet(mapOfMentions.keySet()).toSeq
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
  
  val reader = new EidosSystem()
  
  behavior of "EidosActions"
  
  it should "produce not just unique but also distinct mentions" in {
    val text = "The government promotes improved cultivar to boost agricultural production for ensuring food security."
//    val text = "This is a test"
    val annotatedDocument = reader.extractFrom(text)
    val someMentions = annotatedDocument.mentions
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
    matchingPair should be (None)
  }
}
