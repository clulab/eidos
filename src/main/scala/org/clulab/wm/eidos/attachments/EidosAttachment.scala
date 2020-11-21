package org.clulab.wm.eidos.attachments

import java.time.LocalDateTime

import org.clulab.odin.{Attachment, EventMention, Mention, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.context.{DCT, GeoPhraseID, TimEx, TimeStep}
import org.clulab.wm.eidos.groundings.grounders.{AdjectiveGrounder, AdjectiveGrounding}
import org.clulab.wm.eidos.serialization.jsonld.{JLDAttachment, JLDContextAttachment, JLDScoredAttachment, JLDSerializer, JLDTriggeredAttachment}
import org.clulab.wm.eidos.utils.QuicklyEqualable
import org.json4s.JsonDSL._
import org.json4s._

import scala.annotation.tailrec
import scala.beans.BeanProperty
import scala.util.hashing.MurmurHash3.mix

@SerialVersionUID(1L)
abstract class EidosAttachment extends Attachment with Serializable with QuicklyEqualable {
  implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

  // Support for EidosActions
  @BeanProperty val argumentSize: Int = 0

  // Support for JLD serialization
  def newJLDAttachment(serializer: JLDSerializer): JLDAttachment

  // Support for JSON serialization
  def toJson: JValue

  def groundAdjective(adjectiveGrounder: AdjectiveGrounder): Unit = ()
}

object EidosAttachment {
  val TYPE = "type"
  val TRIGGER = "trigger"
  val QUANTIFICATIONS = "quantifications"

  def newEidosAttachment(mention: Mention): TriggeredAttachment = mention match {
    case eventMention: EventMention => eventMention.label match {
      case Quantification.label => Quantification(eventMention)
      case Increase.label => Increase(eventMention)
      case Decrease.label => Decrease(eventMention)
      case PosChange.label => PosChange(eventMention)
      case NegChange.label => NegChange(eventMention)
    }
  }

  def newEidosAttachment(json: JValue): EidosAttachment = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val kind = (json \ TYPE).extract[String]
    val triggerOpt = (json \ TRIGGER).extractOpt[String]

    triggerOpt.map { trigger =>
      val quantifications: Seq[String] = (json \ QUANTIFICATIONS).extract[Seq[String]]
      val someQuantifications = if (quantifications.nonEmpty) Some(quantifications) else None

      kind match {
        case Increase.label => new Increase(trigger, someQuantifications)
        case Decrease.label => new Decrease(trigger, someQuantifications)
        case PosChange.label => new PosChange(trigger, someQuantifications)
        case NegChange.label => new NegChange(trigger, someQuantifications)
        case Quantification.label => new Quantification(trigger, someQuantifications)
        case Property.label => new Property(trigger, someQuantifications)
        case Hedging.label => new Hedging(trigger, someQuantifications)
        case Negation.label => new Negation(trigger, someQuantifications)
      }
    }
    .getOrElse {
      kind match {
        case Location.label => Location(json)
        case Time.label =>
        // DCTime.label is the same as Time.label, so the cases need to be distinguished by other means.
        // case DCTime.label =>
          // DCTime does not have a text position associated with it, so use the
          // absence of start to distinguish it from Time.
          val start = (json \ "start").extractOpt[String]

          if (start.isEmpty) DCTime(json)
          else Time(json)
        case Score.label => Score(json)
      }
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
      case _: Score => Seq.empty
      case _ => throw new RuntimeException(s"Unsupported class of attachment: ${a.getClass}")
    }
  }
}

case class AttachmentInfo(triggerText: String, quantifierTexts: Option[Seq[String]] = None,
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)


case class Provenance(document: Document, sentence: Int, interval: Interval) extends Comparable[Provenance] {
  override def compareTo(other: Provenance): Int = Provenance.compare(this, other)
}

object Provenance {
  def apply(mention: Mention): Provenance = {
    val document: Document = mention.document
    val sentence: Int = mention.sentence
    val interval: Interval = mention.tokenInterval

    Provenance(document, sentence, interval)
  }

  def compare(left: Provenance, right: Provenance): Int = {
    require(left.document.eq(right.document))

    val leftSentence = left.sentence
    val rightSentence = right.sentence

    if (leftSentence != rightSentence)
      leftSentence - rightSentence
    else {
      val leftStart = left.interval.start
      val rightStart = right.interval.start

      if (leftStart != rightStart)
        leftStart - rightStart
      else {
        val leftEnd = left.interval.end
        val rightEnd = right.interval.end

        if (leftEnd != rightEnd)
          leftEnd - rightEnd
        else
          0
      }
    }
  }
}

@SerialVersionUID(1L)
abstract class TriggeredAttachment(@BeanProperty val trigger: String, @BeanProperty val quantifiers: Option[Seq[String]],
    val triggerProvenance: Option[Provenance] = None, val quantifierProvenances: Option[Seq[Provenance]],
    var adjectiveGroundingsOpt: Option[Seq[Option[AdjectiveGrounding]]] = None) extends EidosAttachment {

  // It is done this way, at least temporarily, so that serialization and comparisons can be made between
  // objects with and without the optional values.
  def getTriggerMention: Option[Provenance] = triggerProvenance

  def getQuantifierMentions: Option[Seq[Provenance]] = quantifierProvenances

  override val argumentSize: Int = if (quantifiers.isDefined) quantifiers.get.size else 0

  override def toString: String = {
    getClass.getSimpleName + "(" + trigger + "," + quantifiers.toString + ")"
  }
  // We keep the original order in adverbs for printing and things,
  // but the sorted version will be used for comparison.
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

    mix(trigger.##, sortedQuantifiers.##)
  }

  def newJLDTriggeredAttachment(serializer: JLDSerializer, kind: String): JLDTriggeredAttachment =
      new JLDTriggeredAttachment(serializer, kind, this)

  def toJson(label: String): JValue = {
    val quants =
      if (quantifiers.isDefined) quantifiers.get.map(quantifier => JString(quantifier))
      else Seq.empty

    (EidosAttachment.TYPE -> label) ~
      (EidosAttachment.TRIGGER -> trigger) ~
      (EidosAttachment.QUANTIFICATIONS -> quants)
  }

  override def groundAdjective(adjectiveGrounder: AdjectiveGrounder): Unit = {
    adjectiveGroundingsOpt = quantifiers.map { quantifiers =>
      quantifiers.map { quantifier =>
        adjectiveGrounder.groundAdjective(quantifier)
      }
    }
  }
}

object TriggeredAttachment {

  // For output, arrange first by class to match gold output.
  def lessThan(left: TriggeredAttachment, right: TriggeredAttachment): Boolean = {
    if (left.getClass.getName != right.getClass.getName)
      left.getClass.getName.compareTo(right.getClass.getName) < 0
    else
      compare(left, right) < 0
  }

  @tailrec
  final def recCompareTo(left: Seq[String], right: Seq[String]): Int =
    if (left.isEmpty || right.isEmpty)
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
          val classDiff = left.getClass.getName.compareTo(right.getClass.getName)

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
    val triggerProvenance: Option[Provenance] = Some(Provenance(triggerMention))
    val triggerText: String = triggerMention.text
    val quantifierMentions: Option[Seq[Mention]] = mention.asInstanceOf[EventMention].arguments.get(key)
    val quantifierTexts: Option[Seq[String]] = quantifierMentions.map(_.map(_.text))
    val quantifierProvenances: Option[Seq[Provenance]] = quantifierMentions.map(_.map(Provenance(_)))

    AttachmentInfo(triggerText, quantifierTexts, triggerProvenance, quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class Quantification(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
      newJLDTriggeredAttachment(serializer, Quantification.kind)

  override def toJson: JValue = toJson(Quantification.label)
}

object Quantification {
  val label = "Quantification"
  val kind = "QUANT"
  val argument = "adverb"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Quantification(trigger, quantifiers)

  def apply(mention: Mention): Quantification = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Quantification(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
        attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class Property(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDTriggeredAttachment(serializer, Property.kind)

  override def toJson: JValue = toJson(Property.label)
}

object Property {
  val label = "Property"
  val kind = "PROP"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Property(trigger, quantifiers)

  def apply(mention: Mention): Property = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Property(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
        attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class Increase(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
      newJLDTriggeredAttachment(serializer, Increase.kind)

  override def toJson: JValue = toJson(Increase.label)
}

object Increase {
  val label = "Increase"
  val kind = "INC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Increase(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
        attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class Decrease(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
      newJLDTriggeredAttachment(serializer, Decrease.kind)

  override def toJson: JValue = toJson(Decrease.label)
}

object Decrease {
  val label = "Decrease"
  val kind = "DEC"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new Decrease(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
        attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class PosChange(trigger: String, quantifiers: Option[Seq[String]],
                triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
  extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDTriggeredAttachment(serializer, PosChange.kind)

  override def toJson: JValue = toJson(PosChange.label)
}

object PosChange {
  val label = "PositiveChange"
  val kind = "POS"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new PosChange(trigger, quantifiers)

  def apply(mention: Mention): PosChange = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new PosChange(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
      attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class NegChange(trigger: String, quantifiers: Option[Seq[String]],
                triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
  extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDTriggeredAttachment(serializer, NegChange.kind)

  override def toJson: JValue = toJson(NegChange.label)
}

object NegChange {
  val label = "NegativeChange"
  val kind = "NEG"
  val argument = "quantifier"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new NegChange(trigger, quantifiers)

  def apply(mention: Mention): NegChange = {
    val attachmentInfo = TriggeredAttachment.getAttachmentInfo(mention, argument)

    new NegChange(attachmentInfo.triggerText, attachmentInfo.quantifierTexts,
      attachmentInfo.triggerProvenance, attachmentInfo.quantifierProvenances)
  }
}

@SerialVersionUID(1L)
class Hedging(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment = newJLDTriggeredAttachment(serializer, Hedging.kind)

  override def toJson: JValue = toJson(Hedging.label)
}

object Hedging {
  val label = "Hedging"
  val kind = "HEDGE"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Hedging(trigger, quantifiers)
}

@SerialVersionUID(1L)
class Negation(trigger: String, quantifiers: Option[Seq[String]],
    triggerProvenance: Option[Provenance] = None, quantifierProvenances: Option[Seq[Provenance]] = None)
    extends TriggeredAttachment(trigger, quantifiers, triggerProvenance, quantifierProvenances) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment = newJLDTriggeredAttachment(serializer, Negation.kind)

  override def toJson: JValue = toJson(Negation.label)
}

object Negation {
  val label = "Negation"
  val kind = "NEGATION"

  def apply(trigger: String, quantifiers: Option[Seq[String]]) = new Negation(trigger, quantifiers)
}

@SerialVersionUID(1L)
abstract class ContextAttachment(val text: String, val value: Object) extends EidosAttachment {

  override def toString: String = {
    getClass.getSimpleName + "(" + text + ")"
  }

  def newJLDContextAttachment(serializer: JLDSerializer, kind: String): JLDContextAttachment =
    new JLDContextAttachment(serializer, kind, this)

  def toJson(label: String): JValue = {
    EidosAttachment.TYPE -> label
  }

  override def biEquals(other: Any): Boolean = {
    val that = other.asInstanceOf[ContextAttachment]

    this.text == that.text
  }

  override protected def calculateHashCode: Int = text.##
}

object ContextAttachment {

  def compare(left: ContextAttachment, right: ContextAttachment): Int =
      left.getClass.getName.compareTo(right.getClass.getName)
}

@SerialVersionUID(1L)
class Time(val interval: TimEx) extends ContextAttachment(interval.text, interval) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDContextAttachment(serializer, Time.kind)

  override def toJson: JValue = {
    val intervals = interval.intervals.map { interval =>
      ("startDate" -> interval.startDate.toString) ~
          ("endDate" -> interval.endDate.toString)
    }

    ("type" -> Time.label) ~
        ("text" -> interval.text) ~
        ("start" -> interval.span.start) ~
        ("end" -> interval.span.end) ~
        ("intervals" -> intervals)
  }

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
      mix(super.calculateHashCode, interval.span.##)
}

object Time {
  val label = "Time"
  val kind = "TIMEX"

  def apply(interval: TimEx) = new Time(interval)

  def apply(json: JValue): Time = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val text = (json \ "text").extract[String]
    val startOffset = (json \ "start").extract[Integer]
    val endOffset = (json \ "end").extract[Integer]
    val intervals = (json \ "intervals").asInstanceOf[JArray].arr.map { jValue =>
      val startDate = (jValue \ "startDate").extract[String]
      val endDate = (jValue \ "endDate").extract[String]
      val localStart = LocalDateTime.parse(startDate)
      val localEnd = LocalDateTime.parse(endDate)
      val timeStep = TimeStep(localStart, localEnd)

      timeStep
    }
    val span = Interval(startOffset, endOffset)
    val timEx = new TimEx(span, intervals, text)

    new Time(timEx)
  }

  def lessThan(left: Time, right: Time): Boolean =
    compare(left, right) < 0

  def compare(left: Time, right: Time): Int = {
    val superDiff = ContextAttachment.compare(left, right)

    if (superDiff != 0)
      superDiff
    else {
      val startDiff = left.interval.span.start - right.interval.span.start

      if (startDiff != 0)
        startDiff
      else {
        val endDiff = left.interval.span.end - right.interval.span.end

        endDiff
      }
    }
  }
}

@SerialVersionUID(1L)
class Location(val geoPhraseID: GeoPhraseID) extends ContextAttachment(geoPhraseID.text, geoPhraseID) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDContextAttachment(serializer, Location.kind)

  override def toJson: JValue = ("type" -> Location.label) ~
      ("text" -> geoPhraseID.text) ~
      ("start" -> geoPhraseID.startOffset) ~
      ("end" -> geoPhraseID.endOffset) ~
      ("geoID" -> geoPhraseID.geonameID)

  override def biEquals(other: Any): Boolean = {
    super.biEquals(other) && {
      val that = other.asInstanceOf[Location]

      this.geoPhraseID == that.geoPhraseID // Case classes support this.
    }
  }

  override protected def calculateHashCode: Int =
      mix(super.calculateHashCode, geoPhraseID.##)
}

object Location {
  val label = "Location"
  val kind = "LocationExp"

  def apply(interval: GeoPhraseID) = new Location(interval)

  def apply(json: JValue): Location = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val text = (json \ "text").extract[String]
    val startOffset = (json \ "start").extract[Integer]
    val endOffset = (json \ "end").extract[Integer]
    val geonameID = (json \ "geoID").extractOpt[String]
    val geoPhraseID = GeoPhraseID(text, geonameID, startOffset, endOffset)

    new Location(geoPhraseID)
  }

  def lessThan(left: Location, right: Location): Boolean =
    compare(left, right) < 0

  def compare(left: Location, right: Location): Int = {
    val superDiff = ContextAttachment.compare(left, right)

    if (superDiff != 0)
      superDiff
    else {
      val startDiff = left.geoPhraseID.startOffset - right.geoPhraseID.startOffset

      if (startDiff != 0)
        startDiff
      else {
        val endDiff = left.geoPhraseID.endOffset - right.geoPhraseID.endOffset

        endDiff
      }
    }
  }
}

@SerialVersionUID(1L)
class DCTime(val dct: DCT) extends ContextAttachment(dct.text, dct) {

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    newJLDContextAttachment(serializer, DCTime.kind)

  override def toJson: JValue = ("type" -> DCTime.label) ~
      ("text" -> dct.text) ~
      ("startTime" -> dct.interval.start.toString) ~
      ("endTime" -> dct.interval.end.toString)
}

object DCTime {
  val label = "Time"
  val kind = "TIMEX"

  def apply(dct: DCT) = new DCTime(dct)

  def apply(json: JValue): DCTime = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val text = (json \ "text").extract[String]
    val startTime = (json \ "startTime").extract[String]
    val endTime = (json \ "endTime").extract[String]
    val startDateTime = LocalDateTime.parse(startTime)
    val endDateTime = LocalDateTime.parse(endTime)
    val interval = SimpleInterval(startDateTime, endDateTime)
    val dct = new DCT(interval, text)

    new DCTime(dct)
  }

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

  override def newJLDAttachment(serializer: JLDSerializer): JLDAttachment =
    new JLDScoredAttachment(serializer, Score.kind, this)

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[Score]) {
      val that = other.asInstanceOf[Score]

      this.score == that.score
    }
    else
      false
  }

  override def toJson: JValue = ("type" -> Score.label) ~
      ("score" -> score)
}

object Score {
  val label = "Same-As"
  val kind = "SCORE"

  def apply(score: Double) = new Score(score)

  def apply(json: JValue): Score = {
    implicit def formats: DefaultFormats.type = org.json4s.DefaultFormats

    val score = (json \ "score").extract[Double]

    new Score(score)
  }
}
