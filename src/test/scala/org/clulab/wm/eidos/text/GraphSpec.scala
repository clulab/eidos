package org.clulab.wm.eidos.text

import scala.collection.Seq

import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.attachments._

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

class AttachmentSpec(val attachment: Attachment) extends GraphSpec {
  protected def toString(quantifiers: Option[Seq[Quantifier]]): String = {
    val stringBuilder = new StringBuilder()
    
    if (quantifiers != None)
      stringBuilder
          .append(", ")
          .append(quantifiers.get.map("Quant: " + _).mkString(", "))
    stringBuilder.toString()
  }  
}

class Quant(quantization: Quantification) extends AttachmentSpec(quantization) {
  override def toString = "+QUANT(" + quantization.quantifier + toString(quantization.adverbs) + ")"
}

object Quant {
  def apply(quantifier: Quantifier) =
      new Quant(Quantification(quantifier, None))

  def apply(quantifier: Quantifier, adverbs: String*): Quant = new Quant(Quantification(quantifier, Option(adverbs.toSeq)))
}

class Dec(decrease: Decrease) extends AttachmentSpec(decrease) {
  override def toString = "+DEC(" + decrease.trigger + toString(decrease.quantifiers) + ")"  
}

object Dec {
  def apply(trigger: String) =
      new Dec(Decrease(trigger, None))
  
  def apply(trigger: String, quantifiers: String*) =
      new Dec(Decrease(trigger, Option(quantifiers.toSeq)))
}

class Inc(increase: Increase) extends AttachmentSpec(increase) {
  override def toString = "+INC(" + increase.trigger + toString(increase.quantifiers) + ")"
}

object Inc {
  def apply(trigger: String) =
    new Inc(Increase(trigger, None))
  
  def apply(trigger: String, quantifiers: String*) =
      new Inc(Increase(trigger, Option(quantifiers.toSeq)))
}    

class Unmarked(unmodified: Unmodified) extends AttachmentSpec(unmodified) {
  override def toString = "+" + unmodified.quantifier
}

object Unmarked {
  def apply(quantifier: Quantifier) =
      new Unmarked(Unmodified(quantifier))
}

class NodeSpec(val nodeText: String, val attachmentSpecs: Set[AttachmentSpec], nodeFilter: NodeSpec.NodeFilter = NodeSpec.trueFilter) extends GraphSpec {
  val attachments = attachmentSpecs.map(_.attachment)
  var mention: Option[Mention] = None
  var tested = false
  var complaints = Seq[String]()
  
  protected def matchAttachments(mention: TextBoundMention): Boolean = {
    val success = mention.attachments == attachments
    
    success
  }

  protected def matchText(mention: TextBoundMention): Boolean = {
    val text = mention.text
    val success = text == nodeText
    
    success
  }
    
  protected def testSpec(mentions: Seq[Mention]): Seq[Mention] = {
    val matches1 = mentions
        .filter(_.isInstanceOf[TextBoundMention])
        .map(_.asInstanceOf[TextBoundMention])
        .filter(matchText)
        .filter(matchAttachments)
    val matches = matches1.zipWithIndex.filter { case (mention, index) => nodeFilter(mention, index, matches1.size) }.map(pair => pair._1)
    
    matches
  }
  
  def test(mentions: Seq[Mention]): Seq[String] = {
    if (!tested) {
      val matches = testSpec(mentions)
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
        .append(if (!attachments.isEmpty) "|" else "")
        
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
  override def test(mentions: Seq[Mention]): Seq[String] = {
    if (!tested) {
      val matches = testSpec(mentions)
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
  
  def test(mentions: Seq[Mention]): Seq[String] = {
    if (!tested) {
      val causeComplaints = cause.test(mentions)
      val effectComplaints = effect.test(mentions)

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

  override def test(mentions: Seq[Mention]): Seq[String] = {
    if (!tested) {
      val causeComplaints = cause.test(mentions)
      val effectComplaints = effect.test(mentions)

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
