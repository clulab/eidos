package org.clulab.wm.eidos.graph

import java.util.{IdentityHashMap => JIdentityHashMap}

import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.utils.QuicklyEqualable

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.mix

class TestResult(val mention: Option[Mention], val complaints: Seq[String])

object TestResult {
  type TestResults = JIdentityHashMap[GraphSpec, TestResult]
}

import TestResult.TestResults

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

abstract class AttachmentSpec extends GraphSpec with QuicklyEqualable {

  protected val matchingClass: Class[_]

  protected def matchClass(attachment: Attachment): Boolean =
      attachment.getClass() == matchingClass
}

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
    if (quantifiers.isDefined)
      stringBuilder
          .append(", ")
          .append(quantifiers.get.map("Quant: " + _).mkString(", "))
    stringBuilder.append(")")
        .toString()
  }

  override def calculateHashCode: Int = mix(trigger.##, sortedQuantifiers.##)

  override def biEquals(other: Any): Boolean = {
    val that = other.asInstanceOf[TriggeredAttachmentSpec]

    this.trigger == that.trigger &&
        this.sortedQuantifiers == that.sortedQuantifiers
  }
}

object TriggeredAttachmentSpec {

  protected def matchAttachment(attachment: TriggeredAttachment, attachmentSpec: TriggeredAttachmentSpec): Boolean = {
    val result = attachment.trigger == attachmentSpec.trigger &&
      attachment.sortedQuantifiers == attachmentSpec.sortedQuantifiers &&
      attachmentSpec.matchClass(attachment)
    result
  }

  @tailrec
  protected def recMatchAttachments(attachments: Set[TriggeredAttachment], attachmentSpecs: Seq[TriggeredAttachmentSpec]): Boolean = {
    if (attachments.isEmpty && attachmentSpecs.isEmpty)
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

  def matchAttachments(mention: Mention, attachmentSpecs: Set[TriggeredAttachmentSpec]): Boolean = {
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

  override def calculateHashCode: Int = text.##

  override def biEquals(other: Any): Boolean = {
    val that = other.asInstanceOf[ContextAttachmentSpec]

    this.text == that.text
  }
}

object ContextAttachmentSpec {

  protected def matchAttachment(attachment: ContextAttachment, attachmentSpec: ContextAttachmentSpec): Boolean = {
    val result = attachment.text == attachmentSpec.text &&
      attachmentSpec.matchClass(attachment)
    result
  }

  @tailrec
  protected def recMatchAttachments(attachments: Set[ContextAttachment], attachmentSpecs: Seq[ContextAttachmentSpec]): Boolean = {
    if (attachments.isEmpty && attachmentSpecs.isEmpty)
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

  def matchAttachments(mention: Mention, attachmentSpecs: Set[ContextAttachmentSpec]): Boolean = {
    if (mention.attachments.count(_.isInstanceOf[ContextAttachment]) != attachmentSpecs.size)
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

  override protected val matchingClass: Class[_] = Quant.targetClass

  override def toString: String = toString(Quant.abbrev)
}

object Quant {
  val abbrev = "QUANT"
  val targetClass: Class[_] = classOf[Quantification]

  def apply(trigger: String) =
      new Quant(trigger, None)

  def apply(trigger: String, quantifiers: String*): Quant = new Quant(trigger, Option(quantifiers.toSeq))
}

class Dec(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {

  override protected val matchingClass: Class[_] = Dec.targetClass

  override def toString: String = toString(Dec.abbrev)
}

object Dec {
  val abbrev = "DEC"
  val targetClass: Class[_] = classOf[Decrease]

  def apply(trigger: String) =
      new Dec(trigger, None)
  
  def apply(trigger: String, quantifiers: String*) =
      new Dec(trigger, Option(quantifiers.toSeq))
}

class Inc(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {

  override protected val matchingClass: Class[_] = Inc.targetClass

  override def toString: String = toString(Inc.abbrev)
}

object Inc {
  val abbrev = "INC"
  val targetClass: Class[_] = classOf[Increase]

  def apply(trigger: String): Inc =
    new Inc(trigger, None)
  
  def apply(trigger: String, quantifiers: String*): Inc =
      new Inc(trigger, Option(quantifiers.toSeq))
}

class TimEx(text: String) extends ContextAttachmentSpec(text) {

  override protected val matchingClass: Class[_] = TimEx.targetClass

  override def toString: String = toString(TimEx.abbrev)
}

object TimEx {
  val abbrev = "TIME"
  val targetClass: Class[_] = classOf[Time]

  def apply(text: String) =  new TimEx(text)
}

class GeoLoc(text: String) extends ContextAttachmentSpec(text) {

  override protected val matchingClass: Class[_] = GeoLoc.targetClass

  override def toString = toString(GeoLoc.abbrev)
}

object GeoLoc {
  val abbrev = "GEO"
  val targetClass: Class[_] = classOf[Location]

  def apply(text: String) =  new GeoLoc(text)
}

class Unmarked(trigger: String, quantifiers: Option[Seq[String]]) extends TriggeredAttachmentSpec(trigger, quantifiers) {

  override protected val matchingClass: Class[_] = Unmarked.targetClass

  override def toString: String = toString(Unmarked.abbrev)
}

object Unmarked {
  val abbrev = ""
  val targetClass: Class[_] = classOf[Unmarked]

  def apply(trigger: String) =
      new Unmarked(trigger, None)

  def apply(trigger: String, quantifiers: String*) =
    new Unmarked(trigger, Option(quantifiers.toSeq))
}

class NodeSpec(val nodeText: String, val attachmentSpecs: Set[AttachmentSpec], nodeFilter: NodeSpec.NodeFilter = NodeSpec.trueFilter) extends GraphSpec {

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
  
  def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean, testResults: TestResults): TestResult = {
    if (!testResults.containsKey(this)) {
      val matches = testSpec(mentions, useTimeNorm, useGeoNorm)
      val testResult =
          if (matches.size < 1)
            new TestResult(None, Seq("Could not find NodeSpec " + this))
          else if (matches.size > 1)
            new TestResult(None, Seq("Found too many (" + matches.size + ") instances of NodeSpec " + this))
          else
            new TestResult(Some(matches.head), Seq.empty)

      testResults.put(this, testResult)
    }
    testResults.get(this)
  }
  
  protected def toString(left: String, right: String): String = {
    val stringBuilder = new StringBuilder(left)
        .append(nodeText)
        .append(if (attachmentSpecs.nonEmpty) "|" else "")
        
    attachmentSpecs.foreach(attachmentSpec => stringBuilder.append(attachmentSpec.toString))
    stringBuilder
        .append(right)
        .toString()
  }
  
  override def toString: String = toString("[", "]")
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

  override def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean, testResults: TestResults): TestResult = {
    if (!testResults.containsKey(this)) {
      val matches = testSpec(mentions, useTimeNorm, useGeoNorm)
      val testResult =
          if (matches.nonEmpty)
            new TestResult(None, Seq("Could find AntiNodeSpec " + this))
          else
            new TestResult(None, Seq.empty)

      testResults.put(this, testResult)
    }
    testResults.get(this)
  }

  override def toString: String = toString("]", "[")
}

object AntiNodeSpec {
  def apply(nodeText: String, attachmentSpecs: Set[AttachmentSpec]) =
      new AntiNodeSpec(nodeText, attachmentSpecs)
  def apply(nodeText: String, attachmentSpecs: AttachmentSpec*) =
      new AntiNodeSpec(nodeText, attachmentSpecs.toSet)  
}


class EdgeSpec(val cause: NodeSpec, val event: EventSpec, val effect: NodeSpec) extends GraphSpec {

  protected def getArgument(eventMention: EventMention, causeOrEffectMention: Option[Mention], argument: String): Option[Mention] = {
    if (eventMention.arguments.contains(argument) && causeOrEffectMention.isDefined)
      eventMention.arguments(argument).find(_ == causeOrEffectMention.get)
    else 
      None
  }
  
  protected def matchCause(causeMention: Option[Mention])(eventMention: EventMention): Boolean =
      getArgument(eventMention, causeMention, "cause").isDefined

  protected def matchEffect(effectMention: Option[Mention])(eventMention: EventMention): Boolean =
      getArgument(eventMention, effectMention, "effect").isDefined

  protected def crossMatchCause(causeMention: Option[Mention])(eventMention: EventMention): Boolean =
    getArgument(eventMention, causeMention, "effect").isDefined

  protected def crossMatchEffect(effectMention: Option[Mention])(eventMention: EventMention): Boolean =
    getArgument(eventMention, effectMention, "cause").isDefined

  protected def testSpec(mentions: Seq[Mention], causeMention: Option[Mention], effectMention: Option[Mention]): Seq[Mention] = {
    val matches1 = mentions
    val matches2 = matches1.filter(_.isInstanceOf[EventMention])
    val matches3 = matches2.map(_.asInstanceOf[EventMention])
    val matches4 = matches3.filter(_.matches(event.label))

    val matches5a = matches4.filter(matchCause(causeMention))
    val matches6a = matches5a.filter(matchEffect(effectMention))
    val matches =
      if (event.directed)
        matches6a
      else {
        val matches5b = matches4.filter(crossMatchCause(causeMention))
        val matches6b = matches5b.filter(crossMatchEffect(effectMention))

        matches6a ++ matches6b
      }

    matches
  }

  def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean, testResults: TestResults): TestResult = {
    if (!testResults.containsKey(this)) {
      val causeTestResult = cause.test(mentions, useTimeNorm, useGeoNorm, testResults)
      val effectTestResult = effect.test(mentions, useTimeNorm, useGeoNorm, testResults)

      val causeComplaints = causeTestResult.complaints
      val effectComplaints = effectTestResult.complaints

      val causeSuccess = causeComplaints.isEmpty
      val effectSuccess = effectComplaints.isEmpty
      val edgeTestResult =
          if (causeSuccess && effectSuccess) {
            val matches = testSpec(mentions, causeTestResult.mention, effectTestResult.mention)

            if (matches.size < 1)
              new TestResult(None, Seq("Could not find EdgeSpec " + this))
            else if (matches.size > 1)
              new TestResult(None, Seq("Found too many (" + matches.size + ") instances of EdgeSpec " + this))
            else
              new TestResult(Some(matches.head), Seq.empty)
          }
          else new TestResult(None, Seq.empty)
      val testResult = new TestResult(edgeTestResult.mention, edgeTestResult.complaints ++ causeComplaints ++ effectComplaints)

      testResults.put(this, testResult)
    }
    testResults.get(this)
  }

  def toString(left: String, right: String): String = {
    new StringBuilder(cause.toString())
        .append(left)
        .append(event.label)
        .append(right)
        .append(effect.toString())
        .toString()
  }
    
  override def toString: String = toString("->(", ")->")
}

object EdgeSpec {
    def apply(cause: NodeSpec, event: EventSpec, effect: NodeSpec) =
      new EdgeSpec(cause, event, effect)
}

class AntiEdgeSpec(cause: NodeSpec, event: EventSpec, effect: NodeSpec) extends EdgeSpec(cause, event, effect) {
  override def toString: String = toString("->)", "(->")

  override def test(mentions: Seq[Mention], useTimeNorm: Boolean, useGeoNorm: Boolean, testResults: TestResults): TestResult = {
    if (!testResults.containsKey(this)) {
      val causeTestResult = cause.test(mentions, useTimeNorm, useGeoNorm, testResults)
      val effectTestResult = effect.test(mentions, useTimeNorm, useGeoNorm, testResults)

      val causeComplaints = causeTestResult.complaints
      val effectComplaints = effectTestResult.complaints

      val causeSuccess = causeComplaints.isEmpty
      val effectSuccess = effectComplaints.isEmpty

      val edgeTestResult =
          if (causeSuccess && effectSuccess) {
            val matches = testSpec(mentions, causeTestResult.mention, effectTestResult.mention)

            if (matches.nonEmpty)
              new TestResult(None, Seq("Could find AntiEdgeSpec " + this))
            else
              new TestResult(None, Seq.empty)
          }
          else new TestResult(None, Seq.empty)
      val testResult = new TestResult(edgeTestResult.mention, edgeTestResult.complaints ++ causeComplaints ++ effectComplaints)

      testResults.put(this, testResult)
    }
    testResults.get(this)
  }
}

object AntiEdgeSpec {
    def apply(cause: NodeSpec, event: EventSpec, effect: NodeSpec) =
      new AntiEdgeSpec(cause, event, effect)
}
