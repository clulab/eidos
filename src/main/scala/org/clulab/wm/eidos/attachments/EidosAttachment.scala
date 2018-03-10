package org.clulab.wm.eidos.attachments

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.EventMention
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.json.odin.{JLDSerializer => JLDOdinSerializer, JLDAttachment => JLDOdinAttachment}
import org.clulab.wm.eidos.serialization.json.{JLDSerializer => JLDEidosSerializer, JLDAttachment => JLDEidosAttachment}

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.Serialization.write

import scala.beans.BeanProperty

@SerialVersionUID(1L)
abstract class EidosAttachment extends Attachment with Serializable {
//  implicit val formats = org.json4s.DefaultFormats
  
  // Support for EidosActions
  def argumentSize: Int
  
  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment
  def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment
  
  // Support for JSON serialization
  def toJson(): JValue

  // Convenience functions
  def argumentSize(optionSeq: Option[Seq[String]]) =
    if (optionSeq.isDefined) optionSeq.size else 0
    
  def toJson(label: String): JValue =
    JNothing
//      (EidosAttachment.TYPE -> label) ~
//          (EidosAttachment.MOD -> write(this))
}

object EidosAttachment {
  val TYPE = "type"
  val MOD = "mod"
  
  def newEidosAttachment(mention: Mention) = mention.label match {
    case Quantification.label => Quantification(mention)
    case Increase.label => Increase(mention)
    case Decrease.label => Decrease(mention)
  }

  def newEidosAttachment(json: JValue) = {
    implicit val formats = org.json4s.DefaultFormats
    
    def parseJValue(jValue: JValue): JValue =
      JsonMethods.parse((jValue \ MOD).extract[String])
 
    (json \ TYPE).extract[String] match {
      case Increase.label => parseJValue(json).extract[Increase]
      case Decrease.label => parseJValue(json).extract[Decrease]
      case Quantification.label => parseJValue(json).extract[Quantification]
    }
  }
  
  def asEidosAttachment(attachment: Attachment): EidosAttachment =
      attachment.asInstanceOf[EidosAttachment]
    
  def getOptionalQuantifiers(mention: Mention): Option[Seq[Quantifier]] =
      mention.asInstanceOf[EventMention]
          .arguments
          .get("quantifier")
          .map(qs => qs.map(_.text))
}

@SerialVersionUID(1L)
case class Quantification(@BeanProperty var quantifier: Quantifier, @BeanProperty var adverbs: Option[Seq[String]]) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(adverbs)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "QUANT", quantifier, adverbs, mention)

  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "QUANT", quantifier, adverbs, mention)

  override def toJson(): JValue = toJson(Quantification.label)
}

object Quantification {
  val label = "Quantification"
  
  def apply(quantifier: Quantifier, adverbs: Option[Seq[String]]): Quantification =
    new Quantification(quantifier, adverbs)
  
  def apply(mention: Mention): Quantification = {
    val quantifier = mention.asInstanceOf[EventMention].trigger.text
    val adverbs = mention.asInstanceOf[EventMention].arguments.get("adverb") match {
      case Some(found) => Some(found.map(_.text))
      case None => None
    }
    
    Quantification(quantifier, adverbs)
  }
}

@SerialVersionUID(1L)
case class Increase(trigger: String, quantifiers: Option[Seq[Quantifier]]) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(quantifiers)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
      new JLDOdinAttachment(serializer, "INC", trigger, quantifiers, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
      new JLDEidosAttachment(serializer, "INC", trigger, quantifiers, mention)

  override def toJson(): JValue = toJson(Increase.label)
}

object Increase {
  val label = "Increase"
  
  def apply(trigger: String, quantifiers: Option[Seq[Quantifier]] = None): Increase =
    new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val trigger = mention.asInstanceOf[EventMention].trigger.text
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    
    Increase(trigger, quantifiers)
  }
}

@SerialVersionUID(1L)
case class Decrease(trigger: String, quantifiers: Option[Seq[Quantifier]] = None) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(quantifiers)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "DEC", trigger, quantifiers, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "DEC", trigger, quantifiers, mention)

  override def toJson(): JValue = toJson(Decrease.label)
}

object Decrease {
  val label = "Decrease"
  
  def apply(trigger: String, quantifiers: Option[Seq[Quantifier]] = None): Decrease =
    new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val trigger = mention.asInstanceOf[EventMention].trigger.text
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)

    Decrease(trigger, quantifiers)
  }
}

case class Score(score: Double) extends EidosAttachment {
  override def argumentSize: Int = 0

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "SCORE", score.toString, None, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "SCORE", score.toString, None, mention)
  
  override def toJson(): JValue = toJson(Score.label)
}

object Score {
  val label = "Same-As"
}
