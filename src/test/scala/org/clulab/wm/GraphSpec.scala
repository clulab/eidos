package org.clulab.wm

import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention

import org.clulab.wm.Aliases.Quantifier

case class Unmarked(quantifier: Quantifier) extends Attachment
  
class Event(val label: String)

object NoEvent extends Event(null)
object Causal extends Event("Causal")
object Correlation extends Event("Correlation")
object IsA extends Event("IsA")
object Origin extends Event("Origin")
object TransparentLink extends Event("TransparentLink")
object Affect extends Event("Affect")

abstract class GraphSpec {
  def toString(mentions: Seq[Mention]): String = {
    val stringBuilder = new StringBuilder()
        .append(mentions.map(_.text).mkString(", "))
         
    if (!mentions.isEmpty && mentions.head.document.discourseTree != None)
      stringBuilder
          .append("\nDiscourse tree: ")
          .append(mentions.head.document.discourseTree.get.toString())
    stringBuilder.toString()
  }
}

class NodeSpec(val nodeText: String, val attachments: Set[Attachment]) extends GraphSpec {
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
    
    println(text)
    success
  }
    
  protected def testSpec(mentions: Seq[Mention]): Option[Mention] = {
    val matches = mentions
        .filter(_.isInstanceOf[TextBoundMention])
        .map(_.asInstanceOf[TextBoundMention])
        .filter(matchText(_))
        .filter(matchAttachments(_))
        
    if (matches.size == 1) Option(matches.head)
    else None
  }
  
  def test(mentions: Seq[Mention]): Seq[String] = {
    if (!tested) {
      mention = testSpec(mentions)
      if (mention == None)
        complaints = Seq("Could not find NodeSpec " + this + " in mentions: " + toString(mentions))
      tested = true
    }
    complaints
  }

  protected def toString(quantifiers: Option[Seq[Quantifier]]): String = {
    val stringBuilder = new StringBuilder()
    
    if (quantifiers != None)
      stringBuilder
          .append(", ")
          .append(quantifiers.get.map(quantifier => "Quant: " + quantifier).mkString(", "))
    stringBuilder.toString()
  }
  
  protected def toString(attachment: Attachment): String = {
    val string = attachment match {
      case x: Decrease => "+DEC(" + x.trigger + toString(x.quantifier)
      case x: Increase => "+INC(" + x.trigger + toString(x.quantifier)
      case x: Quantification => "+QUANT(" + x.quantifier
      case x: Unmarked => "+" + x.quantifier
    }
    string + ")"
  }
  
  override def toString(): String = {
    val stringBuilder = new StringBuilder("[")
        .append(nodeText)
        .append(if (!attachments.isEmpty) "|" else "")
        
    attachments.foreach(attachment => stringBuilder.append(toString(attachment)))
    stringBuilder
        .append("]")
        .toString()
  }
}

class EdgeSpec(val cause: NodeSpec, val event: Event, val effects: Set[NodeSpec]) extends GraphSpec {

  protected def matchArgument(mention: EventMention, argument: String): Boolean = {
    val tmpMentions = cause.mention.get
    
    // Should it match exactly once?  Only a single cause?
    if (mention.arguments.contains(argument))
      mention.arguments(argument).contains(tmpMentions)
    else 
      false
  }
  
  protected def matchCause(mention: EventMention): Boolean =
      matchArgument(mention, "cause")
    
  protected def matchEffects(mention: EventMention): Boolean = { 
    val tmpEffects = effects.map(effect => effect.mention.get) 
 
    // Should it match exactly once? 
    if (mention.arguments.contains("effect")) 
      mention.arguments("effect").toSet == tmpEffects 
    else  
      false 
  } 
  
  protected def testSpec(mentions: Seq[Mention]): Option[Mention] = {
    val matches = mentions
        .filter(_.isInstanceOf[EventMention])
        .map(_.asInstanceOf[EventMention])
        .filter(_.labels.contains(event.label))
        .filter(matchCause(_))
        .filter(matchEffects(_))
    
    if (matches.size == 1) Option(matches.head)
    else None
  }
  
  def test(mentions: Seq[Mention]): Seq[String] = {
    val causeComplaints = cause.test(mentions)
    val effectComplaints = effects.flatMap(effect => effect.test(mentions))

    val causeSuccess =  causeComplaints.isEmpty
    val effectSuccess = effectComplaints.isEmpty
    
    val arrowComplaints =
        if (causeSuccess && effectSuccess && testSpec(mentions) == None)
          Seq("Could not find EdgeSpec " + this + " in mentions: " + toString(mentions))
        else 
          Seq()

    causeComplaints ++ effectComplaints ++ arrowComplaints
  }
  
  override def toString(): String = {
    new StringBuilder(cause.toString())
        .append("->(")
        .append(event.label)
        .append(")->")
        .append(
            if (effects.isEmpty)
              "NoEdge"
            else
              effects.map(effect => effect.toString()).mkString("->")
        )
        .toString()
  }
}
