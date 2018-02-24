package org.clulab.wm.eidos.attachments

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.EventMention
import org.clulab.wm.eidos.Aliases.Quantifier

abstract class EidosAttachment extends Attachment {
  def argumentSize: Int
  
  def argumentSize(optionSeq: Option[Seq[String]]) =
    if (optionSeq.isDefined) optionSeq.size else 0
}

object EidosAttachment {
  
  def newEidosAttachment(mention: Mention) = mention.label match {
    case Quantification.label => Quantification(mention)
    case Increase.label => Increase(mention)
    case Decrease.label => Decrease(mention)
  }
  
  def getOptionalQuantifiers(mention: Mention): Option[Seq[Quantifier]] =
      mention.asInstanceOf[EventMention]
          .arguments
          .get("quantifier")
          .map(qs => qs.map(_.text))
}

case class Quantification(quantifier: Quantifier, adverbs: Option[Seq[String]]) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(adverbs)
}

object Quantification {
  val label = "Quantification"
  
  def apply(quantifier: Quantifier, adverbs: Option[Seq[String]]): Quantification =
    new Quantification(quantifier, adverbs)
  
  def apply(mention: Mention): Quantification = {
    val quantifier = mention.asInstanceOf[EventMention].trigger.text
    val adverbs = mention.asInstanceOf[EventMention].arguments.get("adverb") match {
      case Some(found) => Some(found.map(_.text))
      case None => None
    }
    
    Quantification(quantifier, adverbs)
  }
}

case class Increase(trigger: String, quantifiers: Option[Seq[Quantifier]]) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(quantifiers)
}

object Increase {
  val label = "Increase"
  
  def apply(trigger: String, quantifiers: Option[Seq[Quantifier]] = None): Increase =
    new Increase(trigger, quantifiers)

  def apply(mention: Mention): Increase = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text
    
    Increase(trigger, quantifiers)
  }
}

case class Decrease(trigger: String, quantifiers: Option[Seq[Quantifier]] = None) extends EidosAttachment {
  override def argumentSize: Int = argumentSize(quantifiers)
}

object Decrease {
  val label = "Decrease"
  
  def apply(trigger: String, quantifiers: Option[Seq[Quantifier]] = None): Decrease =
    new Decrease(trigger, quantifiers)

  def apply(mention: Mention): Decrease = {
    val quantifiers = EidosAttachment.getOptionalQuantifiers(mention)
    val trigger = mention.asInstanceOf[EventMention].trigger.text

    Decrease(trigger, quantifiers)
  }
}
