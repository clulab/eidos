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
  implicit val formats = org.json4s.DefaultFormats
  
  // Support for EidosActions
  def argumentSize: Int
  
  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment
  def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment
  
  // Support for JSON serialization
  def toJson(): JValue

  // Convenience functions
  def argumentSize(optionSeq: Option[Seq[String]]) =
    if (optionSeq.isDefined) optionSeq.get.size else 0

  def toJson(label: String): JValue =
      (EidosAttachment.TYPE -> label) ~
          (EidosAttachment.MOD -> write(this))

  def lessThan(other: EidosAttachment): Boolean

  def lessThan(left: Seq[String], right: Seq[String]): Boolean = {
    val result = left.indices.foldLeft(0) { (sign, i) =>
      if (sign != 0) sign
      else left(i).compareTo(right(i))
    }
    result < 0
  }
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

  def lessThan(left: EidosAttachment, right: EidosAttachment): Boolean = {
    if (left.getClass().getName() != right.getClass().getName())
      left.getClass().getName().compareTo(right.getClass().getName()) < 0
    else
      left.lessThan(right)
  }
}

case class Quantification(quantifier: Quantifier, adverbs: Option[Seq[String]]) extends EidosAttachment {
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  protected val sortedArguments: Seq[String] =
      if (!adverbs.isDefined) Seq.empty
      else adverbs.get.sorted

  override def canEqual(other: Any) = other.isInstanceOf[Quantification]

  override def equals(other: scala.Any): Boolean = other match {
    case that: Quantification =>
      that.canEqual(this) &&
          this.quantifier == that.quantifier &&
          this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode = mixLast(quantifier.##, sortedArguments.##)

  override def argumentSize: Int = argumentSize(adverbs)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "QUANT", quantifier, adverbs, mention)

  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "QUANT", quantifier, adverbs, mention)

  override def toJson(): JValue = toJson(Quantification.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Quantification]

    if (this.quantifier != that.quantifier)
      this.quantifier.compareTo(that.quantifier) < 0
    else if (this.sortedArguments.length != that.sortedArguments.length)
      this.sortedArguments.length - that.sortedArguments.length < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Quantification {
  val label = "Quantification"
  
  def apply(mention: Mention): Quantification = {
    val quantifier = mention.asInstanceOf[EventMention].trigger.text
    val adverbs = mention.asInstanceOf[EventMention].arguments.get("adverb") match {
      case Some(found) => Some(found.map(_.text))
      case None => None
    }
    
    Quantification(quantifier, adverbs)
  }
}

case class Increase(trigger: String, quantifiers: Option[Seq[Quantifier]]) extends EidosAttachment {
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  protected val sortedArguments: Seq[String] =
    if (!quantifiers.isDefined) Seq.empty
    else quantifiers.get.sorted

  override def canEqual(other: Any) = other.isInstanceOf[Increase]

  override def equals(other: scala.Any): Boolean = other match {
    case that: Increase =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode = mixLast(trigger.##, sortedArguments.##)

  override def argumentSize: Int = argumentSize(quantifiers)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
      new JLDOdinAttachment(serializer, "INC", trigger, quantifiers, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
      new JLDEidosAttachment(serializer, "INC", trigger, quantifiers, mention)

  override def toJson(): JValue = toJson(Increase.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Increase]

    if (this.trigger != that.trigger)
      this.trigger.compareTo(that.trigger) < 0
    else if (this.sortedArguments.length != that.sortedArguments.length)
      this.sortedArguments.length - that.sortedArguments.length < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Increase {
  val label = "Increase"
  
  def apply(mention: Mention): Increase = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text
    
    Increase(trigger, quantifiers)
  }
}

case class Decrease(trigger: String, quantifiers: Option[Seq[Quantifier]] = None) extends EidosAttachment {
  protected val sortedArguments: Seq[String] =
    if (quantifiers.isEmpty) Seq.empty
    else quantifiers.get.sorted

  override def canEqual(other: Any) = other.isInstanceOf[Decrease]

  override def equals(other: scala.Any): Boolean = other match {
    case that: Decrease =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode = mixLast(trigger.##, sortedArguments.##)

  override def argumentSize: Int = argumentSize(quantifiers)

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "DEC", trigger, quantifiers, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "DEC", trigger, quantifiers, mention)

  override def toJson(): JValue = toJson(Decrease.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Decrease]

    if (this.trigger != that.trigger)
      this.trigger.compareTo(that.trigger) < 0
    else if (this.sortedArguments.length != that.sortedArguments.length)
      this.sortedArguments.length - that.sortedArguments.length < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Decrease {
  val label = "Decrease"
  
  def apply(mention: Mention): Decrease = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text

    Decrease(trigger, quantifiers)
  }
}

case class Score(val score: Double) extends EidosAttachment {
  override def argumentSize: Int = 0

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, "SCORE", score.toString, None, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer, mention: EidosMention): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, "SCORE", score.toString, None, mention)
  
  override def toJson(): JValue = toJson(Score.label)

  def lessThan(other: EidosAttachment): Boolean = score - other.asInstanceOf[Score].score < 0
}

object Score {
  val label = "Same-As"
}
