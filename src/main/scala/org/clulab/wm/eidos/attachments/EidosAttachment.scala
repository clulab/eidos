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

import scala.util.hashing.MurmurHash3.mixLast

abstract class EidosAttachment extends Attachment {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  
  // Support for EidosActions
  def argumentSize: Int
  
  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment
  def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment
  
  // Support for JSON serialization
  def toJson(): JValue

  // Convenience functions
  def argumentSize(optionSeq: Option[Seq[String]]): Int =
      if (optionSeq.isDefined) optionSeq.get.size else 0

  def toJson(label: String): JValue =
      (EidosAttachment.TYPE -> label) ~
          (EidosAttachment.MOD -> write(this))
}

object EidosAttachment {
  val TYPE = "type"
  val MOD = "mod"
  
  def newEidosAttachment(mention: Mention): TriggeredAttachment = mention.label match {
    case Quantification.label => Quantification(mention)
    case Increase.label => Increase(mention)
    case Decrease.label => Decrease(mention)
  }

  def newEidosAttachment(json: JValue): EidosAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
    
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

abstract class TriggeredAttachment(val trigger: String, val quantifiers: Option[Seq[String]]) extends EidosAttachment {
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  protected val sortedArguments: Option[Seq[String]] =
      if (quantifiers.isEmpty) None
      else Some(quantifiers.get.sorted)

  override def argumentSize: Int = argumentSize(quantifiers)

  def canEqual(other: Any): Boolean

  override def equals(other: scala.Any): Boolean = other match {
    case that: TriggeredAttachment =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode: Int = mixLast(trigger.##, sortedArguments.##)

  def newJLDOdinAttachment(serializer: JLDOdinSerializer, kind: String, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, kind, trigger, quantifiers, mention)

  def newJLDEidosAttachment(serializer: JLDEidosSerializer, kind: String, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, kind, trigger, quantifiers, mention)
}

class Quantification(quantifier: String, adverbs: Option[Seq[String]]) extends TriggeredAttachment(quantifier, adverbs) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Quantification]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
      super.newJLDOdinAttachment(serializer, Quantification.kind, mention)

  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
      super.newJLDEidosAttachment(serializer, Quantification.kind, mention)

  override def toJson(): JValue = toJson(Quantification.label)
}

object Quantification {
  val label = "Quantification"
  val kind = "QUANT"

  def apply(quantifier: String, adverbs: Option[Seq[String]]) = new Quantification(quantifier, adverbs)

  def apply(mention: Mention): Quantification = {
    val quantifier = mention.asInstanceOf[EventMention].trigger.text
    val adverbs = mention.asInstanceOf[EventMention].arguments.get("adverb") match {
      case Some(found) => Some(found.map(_.text))
      case None => None
    }
    
    new Quantification(quantifier, adverbs)
  }
}

class Increase(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachment(trigger, quantifiers) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Increase]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
      super.newJLDOdinAttachment(serializer, Increase.kind, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
      super.newJLDEidosAttachment(serializer, Increase.kind, mention)

  override def toJson(): JValue = toJson(Increase.label)
}

object Increase {
  val label = "Increase"
  val kind = "INC"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text

    new Increase(trigger, quantifiers)
  }
}

class Decrease(trigger: String, quantifiers: Option[Seq[String]] = None) extends TriggeredAttachment(trigger, quantifiers) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Decrease]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
      super.newJLDOdinAttachment(serializer, Decrease.kind, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
      super.newJLDEidosAttachment(serializer, Decrease.kind, mention)

  override def toJson(): JValue = toJson(Decrease.label)
}

object Decrease {
  val label = "Decrease"
  val kind= "DEC"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text

    new Decrease(trigger, quantifiers)
  }
}

case class Score(score: Double) extends EidosAttachment {
  override def argumentSize: Int = 0

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, Score.kind, score.toString, None, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, Score.kind, score.toString, None, mention)
  
  override def toJson(): JValue = toJson(Score.label)
}

object Score {
  val label = "Same-As"
  val kind = "SCORE"
}
