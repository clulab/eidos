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

abstract class GraphSpec

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
        complaints = Seq("Could not find NodeSpec " + this)
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
  protected def testPattern = testLines(_)
  //protected def testPattern = testStar(_)
  
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

  protected def matchEffect(mention: EventMention, effect: NodeSpec) =
      getArgument(mention, effect, "effect") != None
    
  protected def matchEffect(mention: EventMention): Boolean =
      effects.exists(effect => matchEffect(mention, effect))
        
  protected def matchEffects(mention: EventMention): Boolean = { 
    val tmpEffects = effects.map(effect => effect.mention.get) 
 
    if (mention.arguments.contains("effect")) 
      // This has to be all of them at once and only all of them
      // Both are converted to sets for comparison
      // If the effects can be a subset, use .sameElements(tmpEfects)
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
        .filter(matchEffects(_)) // All of them
    
    if (matches.size == 1) Option(matches.head)
    else None
  }
    
  protected def testLines(mentions: Seq[Mention]): Seq[String] = {
    val matches = mentions
        .filter(_.isInstanceOf[EventMention])
        .map(_.asInstanceOf[EventMention])
        .filter(_.labels.contains(event.label))
        .filter(matchCause)
        .filter(matchEffect(_)) // One of them
    val badCause = matches.find(mention => getCause(mention).get != cause.mention.get).isDefined
    val effectResults = effects.toSeq
        .map(effect => (effect, matches.find(mention => matchEffect(mention, effect))))
    val complaints = effectResults.flatMap(effectResult =>
      if (effectResult._2.isDefined) Seq()
      else Seq("Could not find line EdgeSpec " + effectResult._1)
    )
    
    if (badCause) complaints ++ Seq("Not all effects had same cause")
    else complaints
  }
    
  protected def testStar(mentions: Seq[Mention]): Seq[String] =
    if (testSpec(mentions) == None)
      Seq("Could not find star EdgeSpec " + this)
    else 
      Seq()
      
  def test(mentions: Seq[Mention]): Seq[String] = {
    val causeComplaints = cause.test(mentions)
    val effectComplaints = effects.flatMap(effect => effect.test(mentions))

    val causeSuccess = causeComplaints.isEmpty
    val effectSuccess = effectComplaints.isEmpty
    
    val edgeComplaints =
        if (causeSuccess && effectSuccess) testPattern(mentions)
        else Seq()            

    causeComplaints ++ effectComplaints ++ edgeComplaints
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
