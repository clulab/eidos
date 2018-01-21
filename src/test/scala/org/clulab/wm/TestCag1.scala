package org.clulab.wm

import org.scalatest._
import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCag1 extends FlatSpec with Matchers {
  type Predicate = (Attachment) => Boolean
  def filterAttachments(mentions: Vector[Mention], predicate: Predicate) = mentions.filter(_.attachments.exists(predicate))

  val mentions = extractMentions(p1s1)
  "p1s1" should "have correct number of mentions" in {
    mentions.size should be (11)
  }
  it should "have correct number of quantifiers" in {
    def predicate: Predicate = _.isInstanceOf[Quantification]
    val quantifieds = filterAttachments(mentions, predicate)
    quantifieds.size should be (2)
    val triggerAndTexts = mentions.flatMap { mention =>
      mention.attachments.filter(predicate).map(attachment => (attachment.asInstanceOf[Quantification].quantifier, mention.text))
    }
    triggerAndTexts.foreach { triggerAndTexts =>
      triggerAndTexts match {
        case (trigger, text) => println(s"[${text}|+QUANT(${trigger})]")
      }
    }
  }
  it should "have correct number of increases" in {
    def predicate: Predicate = _.isInstanceOf[Increase]
    val increasings = filterAttachments(mentions, predicate)
    increasings.size should be (0)
  }
  it should "have correct number of decreases" in {
    def predicate: Predicate = _.isInstanceOf[Decrease]
    val decreasings = filterAttachments(mentions, predicate)
    decreasings.size should be (1)
    val triggerAndTexts = mentions.flatMap { mention =>
      mention.attachments.filter(predicate).map(attachment => (attachment.asInstanceOf[Decrease].trigger,  mention.text))
    }
    triggerAndTexts.foreach { triggerAndTexts =>
      triggerAndTexts match {
        case (trigger, text) => println(s"[${text}|+DEC(${trigger})]")
      }
    }
  }
}
