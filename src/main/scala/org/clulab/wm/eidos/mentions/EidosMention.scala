package org.clulab.wm.eidos.mentions

import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.struct.Interval

trait EidosMention {
  // Add common functionality
}

object EidosMention {
  // Convert from regular mention to any of those below
  
}

abstract class NonEidosMention extends Mention with EidosMention {
  // Use has-a, accept mention in constructor and store
  // Pass all mention functionality to it.
  // Put no-op values here for Eidos functionality.
}

class EidosTextBoundMention(labels: Seq[String], tokenInterval: Interval, sentence: Int,
    document: Document, keep: Boolean, foundBy: String, attachments: Set[Attachment] = Set.empty) 
    extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with EidosMention {
  
//  def this(mention: TextBoundMention) = 
  
  val canonicalForm: String = "hello"
}

class EidosEventMention extends /*EventMention with*/ EidosMention {
//  def this(mention: EventMention) = 
  
}

class EidosRelationMention extends /*RelationMention with*/ EidosMention {
  //  def this(mention: RelationMention) = 

}

class EidosSameAsMention extends EidosMention {
  
}
