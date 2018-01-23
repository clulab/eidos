package org.clulab.wm

import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention

import org.clulab.wm.Aliases.Quantifier

abstract class GraphSpec {
  def toString(mentions: Vector[Mention]): String = {
    mentions.toString()
  }
}

class NodeSpec(val nodeText: String, val attachments: Set[Attachment]) extends GraphSpec {
  var mention: Option[Mention] = None
  var tested = false
  var complaints = Seq[String]()
  
  protected def matchAttachments(mention: TextBoundMention): Boolean = {
    val result = mention.attachments == attachments
    
    result
  }

  protected def matchText(mention: TextBoundMention): Boolean = {
    val text = mention.text
    val result = text == nodeText
    
    println(text)
    result
  }
    
  protected def testSpec(mentions: Vector[Mention]): Option[Mention] = {
    val matches = mentions
        .filter(_.isInstanceOf[TextBoundMention])
        .filter(mention => matchText(mention.asInstanceOf[TextBoundMention]))
        .filter(mention => matchAttachments(mention.asInstanceOf[TextBoundMention]))
        
    if (matches.size == 1) Option(matches.head)
    else None
  }
  
  def test(mentions: Vector[Mention]): Seq[String] = {
    if (!tested) {
      mention = testSpec(mentions)
      if (mention == None)
        complaints = Seq("Could not find NodeSpec " + this + " in mentions " + toString(mentions))
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
    val result = attachment match {
      case x: Decrease => "+DEC(" + x.trigger + toString(x.quantifier)
      case x: Increase => "+INC(" + x.trigger + toString(x.quantifier)
      case x: Quantification => "+QUANT(" + x.quantifier
    }
    result + ")"
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

class EdgeSpec(val cause: NodeSpec, val effects: Set[NodeSpec]) extends GraphSpec {

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
  
  protected def testSpec(mentions: Vector[Mention]): Option[Mention] = {
    val matches = mentions
        .filter(_.isInstanceOf[EventMention]) // so just map it here?
        .filter(mention => matchCause(mention.asInstanceOf[EventMention]))
        .filter(mention => matchEffects(mention.asInstanceOf[EventMention]))
    
    if (matches.size == 1) Option(matches.head)
    else None
  }
  
  def test(mentions: Vector[Mention]): Seq[String] = {
    val causeComplaints = cause.test(mentions)
    val effectComplaints = effects.flatMap(effect => effect.test(mentions))

    val causeResult =  causeComplaints.isEmpty
    val effectResult = effectComplaints.isEmpty
    
    val arrowComplaints =
        if (causeResult && effectResult && testSpec(mentions) == None)
          Seq("Could not find EdgeSpec " + this + " in mentions " + toString(mentions))
        else 
          Seq()

    causeComplaints ++ effectComplaints ++ arrowComplaints
  }
  
  override def toString(): String = {
    new StringBuilder(cause.toString())
        .append("->")
        .append(
            if (effects.isEmpty)
              "NoEdge"
            else
              effects.map(effect => effect.toString()).mkString("->")
        )
        .toString()
  }
}
