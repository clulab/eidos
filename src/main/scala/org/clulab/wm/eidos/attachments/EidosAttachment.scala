package org.clulab.wm.eidos.attachments

import org.clulab.odin.{Attachment, EventMention, Mention, TextBoundMention}
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.json.odin.{JLDAttachment => JLDOdinAttachment, JLDSerializer => JLDOdinSerializer}
import org.clulab.wm.eidos.serialization.json.{JLDTriggeredAttachment => JLDEidosAttachment, JLDSerializer => JLDEidosSerializer}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.Serialization.write

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.mixLast

abstract class EidosAttachment extends Attachment {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  // Support for EidosActions
  def argumentSize: Int = 0

  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment
  def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment

  // Support for JSON serialization
  def toJson(): JValue

  // Convenience functions
  def argumentSize(optionSeq: Option[Seq[String]]): Int =
    if (optionSeq.isDefined) optionSeq.get.size else 0

  def toJson(label: String): JValue =
    (EidosAttachment.TYPE -> label) ~
      (EidosAttachment.MOD -> write(this))

  // Support for EidosMention which returns any mentions hiding in the attachments
  def attachmentMentions: Seq[Mention] = Seq.empty

  def lessThan(other: EidosAttachment): Boolean

  @tailrec
  final def recLessThan(left: Seq[String], right: Seq[String]): Boolean =
    if (left.length == 0 && right.length == 0)
      false
    else {
      val sign = left.head.compareTo(right.head)

      if (sign != 0) sign < 0
      else recLessThan(left.tail, right.tail)
    }

  def lessThan(left: Seq[String], right: Seq[String]): Boolean =
    if (left.length != right.length)
      left.length - right.length < 0
    else
      recLessThan(left, right)
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

  def lessThan(left: EidosAttachment, right: EidosAttachment): Boolean = {
    if (left.getClass().getName() != right.getClass().getName())
      left.getClass().getName().compareTo(right.getClass().getName()) < 0
    else
      left.lessThan(right)
  }
}

abstract class TriggeredAttachment(val trigger: String, val quantifiers: Option[Seq[String]],
                                   val triggerMention: Option[TextBoundMention] = None, val quantifierMentions: Option[Seq[Mention]]) extends EidosAttachment {
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

  def newJLDEidosAttachment(serializer: JLDEidosSerializer, kind: String): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, kind, trigger, quantifiers)

  override def attachmentMentions: Seq[Mention] = {
    val someTriggerMentions =
      if (triggerMention.isEmpty) Seq.empty
      else Seq(triggerMention.get)
    val someQuantifierMentions =
      if (quantifierMentions.isEmpty) Seq.empty
      else quantifierMentions.get

    someTriggerMentions ++ someQuantifierMentions
  }
}

case class AttachmentInfo(triggerMention: TextBoundMention, triggerText: String, quantifierMentions: Option[Seq[Mention]], quantifierTexts: Option[Seq[String]])

object TriggeredAttachment {

  implicit def ordering[T <: TriggeredAttachment]: Ordering[T] = new Ordering[T] {
    def compare(left: T, right: T): Int = {
      val triggerDiff = left.trigger.length - right.trigger.length

      if (triggerDiff != 0)
        triggerDiff
      else
        left.argumentSize - right.argumentSize
    }
  }

  def getAttachmentInfo(mention: Mention, key: String): AttachmentInfo = {
    val triggerMention: TextBoundMention = mention.asInstanceOf[EventMention].trigger
    val triggerText: String = triggerMention.text
    val quantifierMentions: Option[Seq[Mention]] = mention.asInstanceOf[EventMention].arguments.get(key)
    val quantifierTexts: Option[Seq[String]] = quantifierMentions.map(_.map(_.text))

    AttachmentInfo(triggerMention, triggerText, quantifierMentions, quantifierTexts)
  }
}

class Quantification(quantifier: String, adverbs: Option[Seq[String]], quantifierMention: Option[TextBoundMention] = None, adverbMentions: Option[Seq[Mention]] = None)
  extends TriggeredAttachment(quantifier, adverbs, quantifierMention, adverbMentions) {
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  protected val sortedArguments: Seq[String] =
  if (adverbs.isEmpty) Seq.empty
  else adverbs.get.sorted

  override def hashCode = mixLast(quantifier.##, sortedArguments.##)

  override def equals(other: scala.Any): Boolean = other match {
    case that: Quantification =>
      that.canEqual(this) &&
        this.quantifier == that.quantifier &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def argumentSize: Int = argumentSize(adverbs)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Quantification]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    super.newJLDOdinAttachment(serializer, Quantification.kind, mention)

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    super.newJLDEidosAttachment(serializer, Quantification.kind)

  override def toJson(): JValue = toJson(Quantification.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Quantification]

    if (this.quantifier != that.quantifier)
      this.quantifier.compareTo(that.quantifier) < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Quantification {
  val label = "Quantification"
  val kind = "QUANT"
  val argument = "adverb"

  def apply(quantifier: String, adverbs: Option[Seq[String]]) = new Quantification(quantifier, adverbs)

  def apply(mention: Mention): Quantification = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Quantification(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, Some(attachmentInfo.triggerMention), attachmentInfo.quantifierMentions)
  }
}

class Increase(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None, quantifierMentions: Option[Seq[Mention]] = None)
  extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  protected val sortedArguments: Seq[String] =
  if (quantifiers.isEmpty) Seq.empty
  else quantifiers.get.sorted

  override def equals(other: scala.Any): Boolean = other match {
    case that: Increase =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode = mixLast(trigger.##, sortedArguments.##)

  override def argumentSize: Int = argumentSize(quantifiers)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Increase]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    super.newJLDOdinAttachment(serializer, Increase.kind, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    super.newJLDEidosAttachment(serializer, Increase.kind)

  override def toJson(): JValue = toJson(Increase.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Increase]

    if (this.trigger != that.trigger)
      this.trigger.compareTo(that.trigger) < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Increase {
  val label = "Increase"
  val kind = "INC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Increase(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, Some(attachmentInfo.triggerMention), attachmentInfo.quantifierMentions)
  }
}

class Decrease(trigger: String, quantifiers: Option[Seq[String]] = None, triggerMention: Option[TextBoundMention] = None, quantifierMentions: Option[Seq[Mention]] = None)
  extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {
  protected val sortedArguments: Seq[String] =
    if (quantifiers.isEmpty) Seq.empty
    else quantifiers.get.sorted

  override def equals(other: scala.Any): Boolean = other match {
    case that: Decrease =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedArguments == that.sortedArguments
    case _ => false
  }

  override def hashCode = mixLast(trigger.##, sortedArguments.##)

  override def argumentSize: Int = argumentSize(quantifiers)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Decrease]

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    super.newJLDOdinAttachment(serializer, Decrease.kind, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    super.newJLDEidosAttachment(serializer, Decrease.kind)

  override def toJson(): JValue = toJson(Decrease.label)

  def lessThan(other: EidosAttachment): Boolean = {
    val that = other.asInstanceOf[Decrease]

    if (this.trigger != that.trigger)
      this.trigger.compareTo(that.trigger) < 0
    else
      lessThan(this.sortedArguments, that.sortedArguments)
  }
}

object Decrease {
  val label = "Decrease"
  val kind = "DEC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Decrease(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, Some(attachmentInfo.triggerMention), attachmentInfo.quantifierMentions)
  }
}

case class Score(score: Double) extends EidosAttachment {

  override def newJLDAttachment(serializer: JLDOdinSerializer, mention: Mention): JLDOdinAttachment =
    new JLDOdinAttachment(serializer, Score.kind, score.toString, None, mention)
  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    new JLDEidosAttachment(serializer, Score.kind, score.toString, None)

  override def toJson(): JValue = toJson(Score.label)

  def lessThan(other: EidosAttachment): Boolean = score - other.asInstanceOf[Score].score < 0
}

object Score {
  val label = "Same-As"
  val kind = "SCORE"
}