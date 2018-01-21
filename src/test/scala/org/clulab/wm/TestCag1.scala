package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCag1 extends AgroTest {
  type Converter = (Attachment) => String
  type Predicate = (Attachment) => Boolean

  def filterAttachments(mentions: Vector[Mention], predicate: Predicate) = mentions.filter(_.attachments.exists(predicate))
  
  def convertAttachments(mentions: Vector[Mention], predicate: Predicate, converter: Converter, label: String) =
    mentions.flatMap { mention => 
      mention.attachments.filter(predicate).map { attachment =>
        val quantifier = converter(attachment);
        val text = mention.text
        
        s"[${text}|+${label}(${quantifier})]"
      }
    }
  
  val mentions = extractMentions(p1s1)
  "p1s1" should "have correct number of mentions" in {
    mentions.size should be (11)
    // What should they be?
  }
  it should "have the correct quantifiers" in {
    def converter: Converter = _.asInstanceOf[Quantification].quantifier
    def predicate: Predicate = _.isInstanceOf[Quantification]
    
    val expected = Array(
      "[poor rainfall in southeastern areas|+QUANT(poor)]",
      "[cereal production|+QUANT(low)]"
    )
    val actual = convertAttachments(mentions, predicate, converter, "QUANT")
    expected should contain theSameElementsAs actual
    
    val filtered = filterAttachments(mentions, predicate)
    filtered.size should be (2)
  }
  it should "have the correct increases" in {
    def converter: Converter = _.asInstanceOf[Increase].trigger
    def predicate: Predicate = _.isInstanceOf[Increase]
    
    val expected = Array[String]()
    val actual = convertAttachments(mentions, predicate, converter, "INC")
    expected should contain theSameElementsAs actual
    
    val filtered = filterAttachments(mentions, predicate)
    filtered.size should be (0)
  }
  it should "have the correct decreases" in {
    def converter: Converter = _.asInstanceOf[Decrease].trigger
    def predicate: Predicate = _.isInstanceOf[Decrease]
    
    val expected = Array(
      "[cereal production|+DEC(low)]"
    )
    val actual = convertAttachments(mentions, predicate, converter, "DEC")
    expected should contain theSameElementsAs actual

    val filtered = filterAttachments(mentions, predicate)
    filtered.size should be (1)
  }
}
