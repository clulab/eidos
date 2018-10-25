package org.clulab.wm.eidos.text

import scala.collection.Seq
import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.attachments._

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.{mix, mixLast}

case class Unmodified(quantifier: Quantifier) extends Attachment

abstract class GraphSpec

class EventSpec(val label: String, val directed: Boolean) extends GraphSpec

// For testings, should not match anything else
object NoEvent extends EventSpec("", true)
// DirectedRelation
object Causal extends EventSpec("Causal", true)
object IsA extends EventSpec("IsA", true)
object Origin extends EventSpec("Origin", true)
object TransparentLink extends EventSpec("TransparentLink", true)
// UndirectedReleation
object Correlation extends EventSpec("Correlation", false)
object SameAs extends EventSpec("SameAs", false)
// Not in taxonomy
object Affect extends EventSpec("Affect", true)

class AttachmentSpec() extends GraphSpec

abstract class TriggeredAttachmentSpec(val trigger: String, quantifiers: Option[Seq[String]]) extends AttachmentSpec {
  val sortedQuantifiers: Seq[String] =
      if (quantifiers.isEmpty) Seq.empty
      else quantifiers.get.sorted

  protected def toString(abbrev: String): String = {
    val stringBuilder = new StringBuilder()

    stringBuilder
        .append("+")
        .append(abbrev)
        .append("(")
        .append(trigger)
    if (quantifiers != None)
      stringBuilder
          .append(", ")
          .append(quantifiers.get.map("Quant: " + _).mkString(", "))
    stringBuilder.append(")")
        .toString()
  }

  def canEqual(other: Any): Boolean

  override def hashCode: Int = {
    val h0 = getClass().getName().##
    val h1 = mix(h0, trigger.##)

    mixLast(h1, sortedQuantifiers.##)
  }

  override def equals(other: scala.Any): Boolean = other match {
    case that: TriggeredAttachmentSpec =>
      that.canEqual(this) &&
        this.trigger == that.trigger &&
        this.sortedQuantifiers == that.sortedQuantifiers
    case _ => false
  }

  protected def matchClass(attachment: TriggeredAttachment): Boolean
}

object TriggeredAttachmentSpec {

  protected def matchAttachment(attachment: TriggeredAttachment, attachmentSpec: TriggeredAttachmentSpec) = {
    val result = attachment.trigger == attachmentSpec.trigger &&
      attachment.sortedQuantifiers.sameElements(attachmentSpec.sortedQuantifiers) &&
      attachmentSpec.matchClass(attachment)
    result
  }

  @tailrec
  protected def recMatchAttachments(attachments: Set[TriggeredAttachment], attachmentSpecs: Seq[TriggeredAttachmentSpec]): Boolean = {
    if (attachments.size == 0 && attachmentSpecs.size == 0)
      true
    else {
      val attachmentSpec = attachmentSpecs.head
      val attachment = attachments.find(matchAttachment(_, attachmentSpec))
      if (attachment.isEmpty)
        false
      else
        recMatchAttachments(attachments - attachment.get, attachmentSpecs.tail)
    }
  }

  def matchAttachments(mention: Mention, attachmentSpecs: Set[TriggeredAttachmentSpec]) = {
    // For now, let's not write tests for Property attachments as it's likey to be fluid for a bit
    // FIXME: revisit
    def filterCriteria(a: Attachment): Boolean = a.isInstanceOf[TriggeredAttachment] && ! a.isInstanceOf[Property]


    if (mention.attachments.count(filterCriteria) != attachmentSpecs.size)
      false
    else {
      val attachments: Set[TriggeredAttachment] = mention.attachments
          .filter(filterCriteria)
          .map(_.asInstanceOf[TriggeredAttachment])

      recMatchAttachments(attachments, attachmentSpecs.toSeq)
    }
  }
}

abstract class ContextAttachmentSpec(val text: String) extends AttachmentSpec {

 def toString(abbrev: String): String = {
    val stringBuilder = new StringBuilder()

    stringBuilder
      .append("+")
      .append(abbrev)
      .append("(")
      .append(text)
    stringBuilder.append(")")
      .toString()
  }

  def canEqual(other: Any): Boolean

  override def hashCode: Int = {
    val h0 = getClass().getName().##
    val h1 = mix(h0, text.##)

    h1
  }

  override def equals(other: scala.Any): Boolean = other match {
    case that: ContextAttachmentSpec =>
      that.canEqual(this) &&
        this.text == that.text
    case _ => false
  }

  protected def matchClass(attachment: ContextAttachment): Boolean
}

object ContextAttachmentSpec {

  protected def matchAttachment(attachment: ContextAttachment, attachmentSpec: ContextAttachmentSpec) = {
    val result = attachment.text == attachmentSpec.text &&
      attachmentSpec.matchClass(attachment)
    result
  }

  @tailrec
  protected def recMatchAttachments(attachments: Set[ContextAttachment], attachmentSpecs: Seq[ContextAttachmentSpec]): Boolean = {
    if (attachments.size == 0 && attachmentSpecs.size == 0)
      true
    else {
      val attachmentSpec = attachmentSpecs.head
      val attachment = attachments.find(matchAttachment(_, attachmentSpec))
      if (attachment.isEmpty)
        false
      else
        recMatchAttachments(attachments - attachment.get, attachmentSpecs.tail)
    }
  }

  def matchAttachments(mention: Mention, attachmentSpecs: Set[ContextAttachmentSpec]) = {
    if (mention.attachments.filter(_.isInstanceOf[ContextAttachment]).size != attachmentSpecs.size)
      false
    else {
      val attachments: Set[ContextAttachment] = mention.attachments
        .filter(_.isInstanceOf[ContextAttachment])
        .map(_.asInstanceOf[ContextAttachment])

      recMatchAttachments(attachments, attachmentSpecs.toSeq)
    }
  }
}

class Quant(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {
  override def toString = toString(Quant.abbrev)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Quant]

  override protected def matchClass(attachment: TriggeredAttachment): Boolean = attachment match {
    case _: Quantification => true
    case _ => false
  }
}

object Quant {
  val abbrev = "QUANT"

  def apply(trigger: String) =
      new Quant(trigger, None)

  def apply(trigger: String, quantifiers: String*): Quant = new Quant(trigger, Option(quantifiers.toSeq))
}

class Dec(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {
  override def toString = toString(Dec.abbrev)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Dec]

  override protected def matchClass(attachment: TriggeredAttachment): Boolean = attachment match {
    case _: Decrease => true
    case _ => false
  }
}

object Dec {
  val abbrev = "DEC"
  val targetClass = Decrease.getClass()

  def apply(trigger: String) =
      new Dec(trigger, None)
  
  def apply(trigger: String, quantifiers: String*) =
      new Dec(trigger, Option(quantifiers.toSeq))
}

class Inc(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {
  override def toString = toString(Inc.abbrev)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Inc]

  override protected def matchClass(attachment: TriggeredAttachment): Boolean = attachment match {
    case _: Increase => true
    case _ => false
  }
}

object Inc {
  val abbrev = "INC"
  val targetClass = Increase.getClass()

  def apply(trigger: String) =
    new Inc(trigger, None)
  
  def apply(trigger: String, quantifiers: String*) =
      new Inc(trigger, Option(quantifiers.toSeq))
}

class TimEx(text: String) extends ContextAttachmentSpec(text) {
  override def toString = toString(TimEx.abbrev)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[TimEx]

  override protected def matchClass(attachment: ContextAttachment): Boolean = attachment match {
    case _: Time => true
    case _ => false
  }
}

object TimEx {
  val abbrev = "TIME"

  def apply(text: String) =  new TimEx(text)
}

object GeoLoc {
  val abbrev = "GEO"

  def apply(text: String) =  new GeoLoc(text)
}

class GeoLoc(text: String) extends ContextAttachmentSpec(text) {
  override def toString = toString(GeoLoc.abbrev)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[GeoLoc]

  override protected def matchClass(attachment: ContextAttachment): Boolean = attachment match {
    case _: Location => true
    case _ => false
  }
}

class Unmarked(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {
  override def toString = toString("")

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Unmarked]

  override protected def matchClass(attachment: TriggeredAttachment): Boolean = false
}

object Unmarked {
  def apply(trigger: String) =
      new Unmarked(trigger, None)

  def apply(trigger: String, quantifiers: String*) =
    new Unmarked(trigger, Option(quantifiers.toSeq))
}

class NodeSpec(val nodeText: String, val attachmentSpecs: Set[AttachmentSpec], nodeFilter: NodeSpec.NodeFilter = NodeSpec.trueFilter) extends GraphSpec {
  var mention: Option[Mention] = None
  var tested = false
  var complaints = Seq[String]()
  
  protected def matchAttachments(useTimeNorm: Boolean, useGeoNorm: Boolean)(mention: Mention): Boolean =
      TriggeredAttachmentSpec.matchAttachments(mention, attachmentSpecs.filter(_.isInstanceOf[TriggeredAttachmentSpec]).map(_.asInstanceOf[TriggeredAttachmentSpec])) &&
        ((useTimeNorm, useGeoNorm) match {
          case (true, true) =>  ContextAttachmentSpec.matchAttachments(mention, attachmentSpecs.filter(_.isInstanceOf[ContextAttachmentSpec]).map(_.asInstanceOf[ContextAttachmentSpec]))
          case (true, false) => ContextAttachmentSpec.matchAttachments(mention, attachmentSpecs.filter(_.isInstanceOf[TimEx]).map(_.asInstanceOf[ContextAttachmentSpec]))
          case (false, true) => ContextAttachmentSpec.matchAttachments(mention, attachmentSpecs.filter(_.isInstanceOf[GeoLoc]).map(_.asInstanceOf[ContextAttachmentSpec]))
          case _ => true
        })

  protected def matchText(mention: TextBoundMention): Boolean = {
    val text = mention.text
    val success = text == nodeText
    
    success
  }
    
  protected def testSpec(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean): Seq[Mention] = {
    val matches1 = mentions
        .filter(_.isInstanceOf[TextBoundMention])
        .map(_.asInstanceOf[TextBoundMention])
        .filter(matchText)
        .filter(matchAttachments(useTimeNorm, useGeoNorm))

    val matches = matches1.zipWithIndex.filter { case (mention, index) => nodeFilter(mention, index, matches1.size) }.map(pair => pair._1)
    
    matches
  }
  
  def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean): Seq[String] = {
    if (!tested) {
      val matches = testSpec(mentions, useTimeNorm, useGeoNorm)
      if (matches.size < 1)
        complaints = Seq("Could not find NodeSpec " + this)
      else if (matches.size > 1)

        complaints = Seq("Found too many (" + matches.size + ") instances of NodeSpec " + this)

      else
        mention = Some(matches.head)
      tested = true
    }
    complaints
  }
  
  protected def toString(left: String, right: String): String = {
    val stringBuilder = new StringBuilder(left)
        .append(nodeText)
        .append(if (!attachmentSpecs.isEmpty) "|" else "")
        
    attachmentSpecs.foreach(attachmentSpec => stringBuilder.append(attachmentSpec.toString))
    stringBuilder
        .append(right)
        .toString()
  }
  
  override def toString(): String = toString("[", "]")
}

object NodeSpec {
  type NodeFilter = (TextBoundMention, Int, Int) => Boolean
  
  def trueFilter: NodeFilter = (mention: TextBoundMention, index: Int, count: Int) => true
  def firstFilter: NodeFilter = (mention: TextBoundMention, index: Int, count: Int) => index == 0
  def lastFilter: NodeFilter = (mention: TextBoundMention, index: Int, count: Int) => index == count - 1

  def indexOfCount(outerIndex: Int, outerCount: Int): NodeFilter =
      (mention: TextBoundMention, innerIndex: Int, innerCount: Int) => innerIndex == outerIndex && innerCount == outerCount
    
  def apply(nodeText: String, nodeFilter: NodeFilter) =
      new NodeSpec(nodeText, Set(), nodeFilter)
  def apply(nodeText: String, attachmentSpec: AttachmentSpec, nodeFilter: NodeFilter) =
      new NodeSpec(nodeText, Set(attachmentSpec), nodeFilter)
  
  def apply(nodeText: String, attachmentSpecs: Set[AttachmentSpec]) =
      new NodeSpec(nodeText, attachmentSpecs)
  def apply(nodeText: String, attachmentSpecs: AttachmentSpec*) =
      new NodeSpec(nodeText, attachmentSpecs.toSet)  
}

class AntiNodeSpec(nodeText: String, attachmentSpecs: Set[AttachmentSpec]) extends NodeSpec(nodeText, attachmentSpecs) {
  override def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean): Seq[String] = {
    if (!tested) {
      val matches = testSpec(mentions, useTimeNorm, useGeoNorm)
      if (matches.size != 0)
        complaints = Seq("Could find AntiNodeSpec " + this)
      tested = true
    }
    complaints
  }

  override def toString(): String = toString("]", "[")
}

object AntiNodeSpec {
  def apply(nodeText: String, attachmentSpecs: Set[AttachmentSpec]) =
      new AntiNodeSpec(nodeText, attachmentSpecs)
  def apply(nodeText: String, attachmentSpecs: AttachmentSpec*) =
      new AntiNodeSpec(nodeText, attachmentSpecs.toSet)  
}

class EdgeSpec(val cause: NodeSpec, val event: EventSpec, val effect: NodeSpec) extends GraphSpec {
  var mention: Option[Mention] = None
  var tested = false
  var complaints = Seq[String]()

  protected def getArgument(mention: EventMention, nodeSpec: NodeSpec, argument: String): Option[Mention] = {
    val tmpMention = nodeSpec.mention.get

    if (mention.arguments.contains(argument))
      mention.arguments(argument).find(_ == tmpMention)
    else 
      None
  }
    
  protected def getCause(mention: EventMention): Option[Mention] =
      getArgument(mention, cause, "cause")

  protected def getEffect(mention: EventMention, effect: NodeSpec): Option[Mention] =
      getArgument(mention, effect, "effect")
  
  protected def matchCause(mention: EventMention): Boolean =
      getArgument(mention, cause, "cause") != None

  protected def matchEffect(mention: EventMention) =
      getArgument(mention, effect, "effect") != None

  protected def crossMatchCause(mention: EventMention): Boolean =
    getArgument(mention, cause, "effect") != None

  protected def crossMatchEffect(mention: EventMention) =
    getArgument(mention, effect, "cause") != None

  protected def testSpec(mentions: Seq[Mention]): Seq[Mention] = {
    val matches1 = mentions
    val matches2 = matches1.filter(_.isInstanceOf[EventMention])
    val matches3 = matches2.map(_.asInstanceOf[EventMention])
    val matches4 = matches3.filter(_.matches(event.label))
    val matches5a = matches4.filter(matchCause)
    val matches6a = matches5a.filter(matchEffect)
    val matches =
      if (event.directed)
        matches6a
      else {
        val matches5b = matches4.filter(crossMatchCause)
        val matches6b = matches5b.filter(crossMatchEffect)

        matches6a ++ matches6b
      }

    matches
  }
  
  def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean): Seq[String] = {
    if (!tested) {
      val causeComplaints = cause.test(mentions, useTimeNorm, useGeoNorm)
      val effectComplaints = effect.test(mentions, useTimeNorm, useGeoNorm)

      val causeSuccess = causeComplaints.isEmpty
      val effectSuccess = effectComplaints.isEmpty

      val edgeComplaints =
          if (causeSuccess && effectSuccess) {
            val matches = testSpec(mentions)

            tested = true
            if (matches.size < 1)
              Seq("Could not find EdgeSpec " + this)
            else if (matches.size > 1)
              Seq("Found too many (" + matches.size + ") instances of EdgeSpec " + this)
            else {
              mention = Some(matches.head)
              Seq.empty
            }
          }
          else Seq.empty
      complaints = causeComplaints ++ effectComplaints ++ edgeComplaints
    }
    complaints
  }

  def toString(left: String, right: String): String = {
    new StringBuilder(cause.toString())
        .append(left)
        .append(event.label)
        .append(right)
        .append(effect.toString())
        .toString()
  }
    
  override def toString(): String = toString("->(", ")->")
}

object EdgeSpec {
    def apply(cause: NodeSpec, event: EventSpec, effect: NodeSpec) =
      new EdgeSpec(cause, event, effect)
}

class AntiEdgeSpec(cause: NodeSpec, event: EventSpec, effect: NodeSpec) extends EdgeSpec(cause, event, effect) {
  override def toString(): String = toString("->)", "(->")

  override def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean): Seq[String] = {
    if (!tested) {
      val causeComplaints = cause.test(mentions, useTimeNorm, useGeoNorm)
      val effectComplaints = effect.test(mentions, useTimeNorm, useGeoNorm)

      val causeSuccess = causeComplaints.isEmpty
      val effectSuccess = effectComplaints.isEmpty

      val edgeComplaints =
        if (causeSuccess && effectSuccess) {
          val matches = testSpec(mentions)

          tested = true
          if (matches.size != 0)
            Seq("Could find AntiEdgeSpec " + this)
          else
            Seq.empty
        }
        else Seq.empty
      complaints = causeComplaints ++ effectComplaints ++ edgeComplaints
    }
    complaints
  }
}

object AntiEdgeSpec {
    def apply(cause: NodeSpec, event: EventSpec, effect: NodeSpec) =
      new AntiEdgeSpec(cause, event, effect)
}
