package org.clulab.wm

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.odin.EventMention

abstract class GraphSpec

class NodeSpec(val nodeText: String, val attachments: Set[Attachment]) extends GraphSpec {
  var mention: Option[Mention] = None
  var tested = false
  var passed = false
  
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
  
  def test(mentions: Vector[Mention]): Boolean = {
    if (!tested) {
      mention = testSpec(mentions)
      passed = mention != None
      tested = true
    }
    passed
  }
}

class EdgeSpec(val cause: NodeSpec, val effect: NodeSpec) extends GraphSpec {

  protected def matchCause(mention: EventMention): Boolean = {
    val tmpCause = cause.mention.get
    
    if (mention.arguments.contains("cause"))
      mention.arguments("cause").contains(tmpCause)
    else 
      false
  }
    
  protected def matchEffect(mention: EventMention): Boolean = {
    val tmpEffect = effect.mention.get

    if (mention.arguments.contains("effect"))
      mention.arguments("effect").contains(tmpEffect)
    else 
      false
  }
  
  protected def testSpec(mentions: Vector[Mention]): Option[Mention] = {
    val matches = mentions
        .filter(_.isInstanceOf[EventMention]) // so just map it here?
        .filter(mention => matchCause(mention.asInstanceOf[EventMention]))
        .filter(mention => matchEffect(mention.asInstanceOf[EventMention]))
    
    if (matches.size == 1) Option(matches.head)
    else None
  }
  
  def test(mentions: Vector[Mention]): Boolean = {
    val causeResult = cause.test(mentions)
    val effectResult = effect.test(mentions)
    val arrowResult = causeResult && effectResult && testSpec(mentions) != None
    
    (causeResult, effectResult, arrowResult) == (true, true, true)
  }
}
