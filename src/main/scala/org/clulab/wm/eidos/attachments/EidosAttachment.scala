package org.clulab.wm.eidos.attachments

import org.clulab.odin.{Attachment, EventMention, Mention, TextBoundMention}
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.serialization.json.{
  JLDAttachment => JLDEidosAttachment,
  JLDScoredAttachment => JLDEidosScoredAttachment,
  JLDTriggeredAttachment => JLDEidosTriggeredAttachment,
  JLDSerializer => JLDEidosSerializer
}
import org.json4s._
import org.json4s.JsonDSL._

import scala.beans.BeanProperty
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.{mix, mixLast}

@SerialVersionUID(1L)
abstract class EidosAttachment extends Attachment with Serializable {
  implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

  // Support for EidosActions
  @BeanProperty val argumentSize: Int = 0

  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment

  // Support for JSON serialization
  def toJson(): JValue

  // Support for EidosMention which returns any mentions hiding in the attachments
  def attachmentMentions: Seq[Mention] = Seq.empty
}

object EidosAttachment {
  val TYPE = "type"
  val TRIGGER = "trigger"
  val QUANTIFICATIONS = "quantifications"

  def newEidosAttachment(mention: Mention): TriggeredAttachment = {
    val eventMention = mention.asInstanceOf[EventMention]

    eventMention.label match {
      case Quantification.label => Quantification(eventMention)
      case Increase.label => Increase(eventMention)
      case Decrease.label => Decrease(eventMention)
    }
  }

  def newEidosAttachment(json: JValue): EidosAttachment = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val trigger: String = (json \ TRIGGER).extract[String]
    val quantifications: Seq[String] = (json \ QUANTIFICATIONS).extract[Seq[String]]
    val someQuantifications = if (quantifications.nonEmpty) Some(quantifications) else None

    (json \ TYPE).extract[String] match {
      case Increase.label => new Increase(trigger, someQuantifications)
      case Decrease.label => new Decrease(trigger, someQuantifications)
      case Quantification.label => new Quantification(trigger, someQuantifications)
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

case class AttachmentInfo(triggerText: String, quantifierTexts: Option[Seq[String]] = None,
    triggerMention: Option[TextBoundMention] = None, quantifierMentions: Option[Seq[Mention]] = None)

abstract class TriggeredAttachment(@BeanProperty val trigger: String, @BeanProperty val quantifiers: Option[Seq[String]],
    triggerMention: Option[TextBoundMention] = None, quantifierMentions: Option[Seq[Mention]]) extends EidosAttachment {

  // It is done this way, at least temporarily, so that serialization and comparisons can be made between
  // objects with and without the optional values.
  def getTriggerMention = triggerMention

  def getQuantifierMentions = quantifierMentions

  override val argumentSize: Int = if (quantifiers.isDefined) quantifiers.get.size else 0

  override def toString() = {
    getClass().getSimpleName() + "(" + trigger + "," + quantifiers.toString + ")"
  }
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
  // kwa: this is hopefully not used yet
  @BeanProperty val sortedQuantifiers: Seq[String] =
      if (quantifiers.isEmpty) Seq.empty
      else quantifiers.get.sorted

  def canEqual(other: Any): Boolean

  override def equals(other: Any): Boolean = other match {
    case that: TriggeredAttachment =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedQuantifiers == that.sortedQuantifiers
    case _ => false
  }

  override def hashCode: Int = {
    val h0 = getClass().getName().##
    val h1 = mix(h0, trigger.##)

    mixLast(h1, sortedQuantifiers.##)
  }

  def newJLDTriggeredAttachment(serializer: JLDEidosSerializer, kind: String): JLDEidosTriggeredAttachment =
    new JLDEidosTriggeredAttachment(serializer, kind, this)

  override def attachmentMentions: Seq[Mention] = {
    val someTriggerMentions =
        if (triggerMention.isEmpty) Seq.empty
        else Seq(triggerMention.get)
    val someQuantifierMentions =
        if (quantifierMentions.isEmpty) Seq.empty
        else quantifierMentions.get

    someTriggerMentions ++ someQuantifierMentions
  }
  def toJson(label: String): JValue = {
    val quants =
      if (quantifiers.isDefined) quantifiers.get.map(quantifier => JString(quantifier))
      else Seq.empty

    (EidosAttachment.TYPE -> label) ~
      (EidosAttachment.TRIGGER -> trigger) ~
      (EidosAttachment.QUANTIFICATIONS -> quants)
  }
}

object TriggeredAttachment {

  // For output, arrange first by class to match gold output.
  def lessThan(left: TriggeredAttachment, right: TriggeredAttachment): Boolean = {
    if (left.getClass().getName() != right.getClass().getName())
      left.getClass().getName().compareTo(right.getClass().getName()) < 0
    else
      compare(left, right) < 0
  }

  @tailrec
  final def recCompareTo(left: Seq[String], right: Seq[String]): Int =
    if (left.length == 0 || right.length == 0)
      0
    else {
      val headDiff = left.head.compareTo(right.head)

      if (headDiff != 0) headDiff
      else recCompareTo(left.tail, right.tail)
    }

  def compare(left: TriggeredAttachment, right: TriggeredAttachment): Int = {
    val triggerDiff = left.trigger.length - right.trigger.length

    if (triggerDiff != 0)
      triggerDiff
    else {
      val argumentsDiff = left.argumentSize - right.argumentSize

      if (argumentsDiff != 0)
        argumentsDiff
      else {
        val triggerDiff2 = left.trigger.compareTo(right.trigger)

        if (triggerDiff2 != 0)
          triggerDiff2
        else {
          // They could be of different classes and then the first picked would depend on order.
          // This would result in a different mention being picked as best.  It happens!
          val classDiff = left.getClass().getName().compareTo(right.getClass.getName())

          if (classDiff != 0)
            classDiff
          else
            recCompareTo(left.sortedQuantifiers, right.sortedQuantifiers)
        }
      }
    }
  }

  implicit def ordering[T <: TriggeredAttachment]: Ordering[T] = new Ordering[T] {
    def compare(left: T, right: T): Int = TriggeredAttachment.compare(left, right)
  }

  def getAttachmentInfo(mention: Mention, key: String): AttachmentInfo = {
    val triggerMention: TextBoundMention = mention.asInstanceOf[EventMention].trigger
    val triggerText: String = triggerMention.text
    val quantifierMentions: Option[Seq[Mention]] = mention.asInstanceOf[EventMention].arguments.get(key)
    val quantifierTexts: Option[Seq[String]] = quantifierMentions.map(_.map(_.text))

    AttachmentInfo(triggerText, quantifierTexts, Some(triggerMention), quantifierMentions)
  }
}

class Quantification(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Quantification]

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
      newJLDTriggeredAttachment(serializer, Quantification.kind)

  override def toJson(): JValue = toJson(Quantification.label)
}

object Quantification {
  val label = "Quantification"
  val kind = "QUANT"
  val argument = "adverb"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Quantification(trigger, quantifiers)

  def apply(mention: Mention): Quantification = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Quantification(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, attachmentInfo.triggerMention, attachmentInfo.quantifierMentions)
  }
}

class Increase(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Increase]

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
      newJLDTriggeredAttachment(serializer, Increase.kind)

  override def toJson(): JValue = toJson(Increase.label)
}

object Increase {
  val label = "Increase"
  val kind = "INC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Increase(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, attachmentInfo.triggerMention, attachmentInfo.quantifierMentions)
  }
}

class Decrease(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Decrease]

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
      newJLDTriggeredAttachment(serializer, Decrease.kind)

  override def toJson(): JValue = toJson(Decrease.label)
}

object Decrease {
  val label = "Decrease"
  val kind = "DEC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Decrease(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, attachmentInfo.triggerMention, attachmentInfo.quantifierMentions)
  }
}

case class Score(score: Double) extends EidosAttachment {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    new JLDEidosScoredAttachment(serializer, Score.kind, this)

  override def toJson(): JValue = JNull // toJson(Score.label)
}

object Score {
  val label = "Same-As"
  val kind = "SCORE"
}
