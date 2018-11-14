package org.clulab.wm.eidos.attachments

import org.clulab.odin.{Attachment, EventMention, Mention, TextBoundMention}
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.{DCT, TimeInterval}
import org.clulab.wm.eidos.serialization.json.{JLDAttachment => JLDEidosAttachment, JLDContextAttachment => JLDEidosContextAttachment, JLDScoredAttachment => JLDEidosScoredAttachment, JLDSerializer => JLDEidosSerializer, JLDTriggeredAttachment => JLDEidosTriggeredAttachment}
import org.clulab.wm.eidos.utils.QuicklyEqualable
import org.json4s._
import org.json4s.JsonDSL._

import scala.beans.BeanProperty
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.mix

@SerialVersionUID(1L)
abstract class EidosAttachment extends Attachment with Serializable with QuicklyEqualable {
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
      case Property.label => new Property(trigger, someQuantifications)
      case Hedging.label => new Hedging(trigger, someQuantifications)
      case Negation.label => new Negation(trigger, someQuantifications)
    }
  }

  def asEidosAttachment(attachment: Attachment): EidosAttachment =
    attachment.asInstanceOf[EidosAttachment]

  def getOptionalQuantifiers(mention: Mention): Option[Seq[Quantifier]] =
    mention.asInstanceOf[EventMention]
      .arguments
      .get("quantifier")
      .map(qs => qs.map(_.text))

  def getAttachmentWords(a: Attachment): Seq[String] = {
    a match {
      case triggered: TriggeredAttachment => Seq(triggered.trigger) ++ triggered.quantifiers.getOrElse(Seq())
      case context: ContextAttachment => context.text.split(" ")
      case _ => throw new RuntimeException(s"Unsupported class of attachment: ${a.getClass}")
    }
  }
}

case class AttachmentInfo(triggerText: String, quantifierTexts: Option[Seq[String]] = None,
    triggerMention: Option[TextBoundMention] = None, quantifierMentions: Option[Seq[Mention]] = None)

@SerialVersionUID(1L)
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

  override def biEquals(other: Any): Boolean = {
    val that = other.asInstanceOf[TriggeredAttachment]

    this.trigger == that.trigger &&
        this.sortedQuantifiers == that.sortedQuantifiers
  }

  override protected def calculateHashCode: Int = {
    // Since the class is checked in canEqual, it is not needed here.
    //val h0 = getClass().getName().##
    //val h1 = mix(h0, trigger.##)

    mix(trigger.hashCode, sortedQuantifiers.hashCode)
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

@SerialVersionUID(1L)
class Quantification(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

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

@SerialVersionUID(1L)
class Property(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
                     quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    newJLDTriggeredAttachment(serializer, Property.kind)

  override def toJson(): JValue = toJson(Property.label)
}

object Property {
  val label = "Property"
  val kind = "PROP"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Property(trigger, quantifiers)

  def apply(mention: Mention): Property = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Property(attachmentInfo.triggerText, attachmentInfo.quantifierTexts, attachmentInfo.triggerMention, attachmentInfo.quantifierMentions)
  }
}

@SerialVersionUID(1L)
class Increase(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

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

@SerialVersionUID(1L)
class Decrease(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
    quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

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

@SerialVersionUID(1L)
class Hedging(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
              quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment = newJLDTriggeredAttachment(serializer, Hedging.kind)

  override def toJson(): JValue = toJson(trigger)
}

object Hedging {
  val label = "Hedging"
  val kind = "HEDGE"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Hedging(trigger, quantifiers)

}

@SerialVersionUID(1L)
class Negation(trigger: String, quantifiers: Option[Seq[String]], triggerMention: Option[TextBoundMention] = None,
               quantifierMentions: Option[Seq[Mention]] = None) extends TriggeredAttachment(trigger, quantifiers, triggerMention, quantifierMentions) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment = newJLDTriggeredAttachment(serializer, Negation.kind)

  override def toJson(): JValue = toJson(trigger)
}


object Negation {
  val label = "Negation"
  val kind = "NEGATION"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Negation(trigger, quantifiers)
}

@SerialVersionUID(1L)
abstract class ContextAttachment(val text: String, val value: Object) extends EidosAttachment {

  override def toString() = {
    getClass().getSimpleName() + "(" + text + ")"
  }

  def newJLDContextAttachment(serializer: JLDEidosSerializer, kind: String): JLDEidosContextAttachment =
    new JLDEidosContextAttachment(serializer, kind, this)

  def toJson(label: String): JValue = {
    (EidosAttachment.TYPE -> label)
  }

  override def biEquals(other: Any): Boolean = {
    val that = other.asInstanceOf[ContextAttachment]

    this.text == that.text
  }

  override protected def calculateHashCode: Int = text.hashCode
}

object ContextAttachment {

  def compare(left: ContextAttachment, right: ContextAttachment): Int =
      left.getClass().getName().compareTo(right.getClass().getName())
}

@SerialVersionUID(1L)
class Time(val interval: TimeInterval) extends ContextAttachment(interval.text, interval) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    newJLDContextAttachment(serializer, Time.kind)

  override def toJson(): JValue = toJson(Time.label)

  override def biEquals(other: Any): Boolean = {
    super.biEquals(other) && {
      val that = other.asInstanceOf[Time]

      this.interval.span == that.interval.span // &&
          // interval.text is already taken care of in super.
          // Assume that same span results in same intervals.
          // this.interval.intervals == that.interval.intervals
    }
  }

  override protected def calculateHashCode: Int =
      mix(super.calculateHashCode, interval.span.hashCode)
}

object Time {
  val label = "Time"
  val kind = "TIMEX"

  def apply(interval: TimeInterval) = new Time(interval)

  def lessThan(left: Time, right: Time): Boolean =
    compare(left, right) < 0

  def compare(left: Time, right: Time): Int = {
    val superDiff = ContextAttachment.compare(left, right)

    if (superDiff != 0)
      superDiff
    else {
      val startDiff = left.interval.span._1 - right.interval.span._1

      if (startDiff != 0)
        startDiff
      else {
        val endDiff = left.interval.span._2 - right.interval.span._2

        endDiff
      }
    }
  }
}

@SerialVersionUID(1L)
class Location(val location_phraseID: GeoPhraseID) extends ContextAttachment(location_phraseID.phraseID, location_phraseID) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    newJLDContextAttachment(serializer, Location.kind)

  override def toJson(): JValue = toJson(Location.label)

  override def biEquals(other: Any): Boolean = {
    super.biEquals(other) && {
      val that = other.asInstanceOf[Location]

      this.location_phraseID == that.location_phraseID // Case classes support this.
    }
  }

  override protected def calculateHashCode: Int =
      mix(super.calculateHashCode, location_phraseID.hashCode)
}

object Location {
  val label = "Location"
  val kind = "LocationExp"

  def apply(interval: GeoPhraseID) = new Location(interval)

  def lessThan(left: Location, right: Location): Boolean =
    compare(left, right) < 0

  def compare(left: Location, right: Location): Int = {
    val superDiff = ContextAttachment.compare(left, right)

    if (superDiff != 0)
      superDiff
    else {
      val startDiff = left.location_phraseID.StartOffset_locs - right.location_phraseID.StartOffset_locs

      if (startDiff != 0)
        startDiff
      else {
        val endDiff = left.location_phraseID.EndOffset_locs - right.location_phraseID.EndOffset_locs

        endDiff
      }
    }
  }
}

@SerialVersionUID(1L)
class DCTime(val dct: DCT) extends ContextAttachment(dct.text, dct) {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    newJLDContextAttachment(serializer, DCTime.kind)

  override def toJson(): JValue = toJson(DCTime.label)
}

object DCTime {
  val label = "Time"
  val kind = "TIMEX"

  def apply(dct: DCT) = new DCTime(dct)

  def lessThan(left: DCTime, right: DCTime): Boolean =
    compare(left, right) < 0

  def compare(left: DCTime, right: DCTime): Int = {
    val superDiff = ContextAttachment.compare(left, right)

    if (superDiff != 0)
      superDiff
    else
      left.text.compareTo(right.text)
  }
}

@SerialVersionUID(1L)
class Score(val score: Double) extends EidosAttachment {

  override def newJLDAttachment(serializer: JLDEidosSerializer): JLDEidosAttachment =
    new JLDEidosScoredAttachment(serializer, Score.kind, this)

  override def toJson(): JValue = JNull // toJson(Score.label)
}

object Score {
  val label = "Same-As"
  val kind = "SCORE"

  def apply(score: Double) = new Score(score)
}
