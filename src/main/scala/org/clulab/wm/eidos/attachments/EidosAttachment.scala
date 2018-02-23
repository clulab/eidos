package org.clulab.wm.eidos.attachments

import org.clulab.odin.Attachment
import org.clulab.wm.eidos.Aliases.Quantifier

class EidosAttachment extends Attachment {
  
}

case class Quantification(quantifier: Quantifier, adverbs: Option[Seq[String]]) extends EidosAttachment
case class Increase(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends EidosAttachment
case class Decrease(trigger: String, quantifier: Option[Seq[Quantifier]] = None) extends EidosAttachment
