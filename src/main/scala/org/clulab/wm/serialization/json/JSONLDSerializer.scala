package org.clulab.wm.serialization.json

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.odin.RelationMention
import org.clulab.odin.EventMention

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.JSONSerialization
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Interval
import org.clulab.wm.Aliases.Quantifier
import org.clulab.wm.{Quantification, Increase, Decrease}

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

abstract class JSONObjectProvider extends JSONSerialization {
  
  def toJObject: JObject
  
  def jsonAST: JValue = render(toJObject)

  // TODO: Make these nicer by handing out ids from somewhere
  def id(any: Any): String = System.identityHashCode(any).toString
  
  // TODO: Documents may already have some kind of ID.  Should it be used?
  def id(document: Document): String = document.id.getOrElse(id(document.asInstanceOf[Any]))

  def mkId(any: Any) = ("@id" -> (JSONObjectProvider.home + "id/" + id(any)))

  def mkId(document: Document) = ("@id" -> id(document))
  
  def mkType(value: String) = ("@type" -> value)
  
  def mkContext():JObject = {
    def mkContext(name: String) = (name -> (JSONObjectProvider.home + name))
    
    ("@context" ->
        mkContext(JSONDocument.typename) ~
        mkContext(JSONSentence.typename) ~
        mkContext(JSONWord.typename) ~
        mkContext(JSONMention.typename) ~
        mkContext(JSONProvenance.typename) ~
        mkContext(JSONInterval.typename) ~
        mkContext(JSONNamedArgument.typename) 
    )
  }
  
  def newJSONMention(mention: Mention): JSONMention = mention match {
    case mention: TextBoundMention => new JSONTextBoundMention(mention)
    case mention: RelationMention => new JSONRelationMention(mention)
    case mention: EventMention => new JSONEventMention(mention)
  }      

  def newJSONAttachment(attachment: Attachment): JSONAttachment = attachment match {
    // These attachments are the first class in the matching package and could be changed
    case attachment: Quantification => new JSONAttachment("QUANT", attachment.quantifier, None)
    case attachment: Increase => new JSONAttachment("INC", attachment.trigger, attachment.quantifier)
    case attachment: Decrease => new JSONAttachment("DEC", attachment.trigger, attachment.quantifier)
  }
}

object JSONObjectProvider {
  val home = "http://www.example.com/"
}

class JSONNamedArgument(name: String, attachments: Seq[Mention]) extends JSONObjectProvider {

  def toJObject: JObject =
      mkType("Argument") ~
      ("name" -> name) ~
      ("attachments" -> attachments.map(newJSONMention(_).toJObject)) // TODO: Why not id?
}

object JSONNamedArgument {
  val singular = "argument"
  val plural = "arguments"
  val typename = "NamedArgument"
}

class JSONAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]]) extends JSONObjectProvider {
  
  def toJObject: JObject =
      mkType(kind) ~
      ("text", text) ~
      ("modifiers", modifiers) // TODO: This is as far as it goes
}

class JSONInterval(interval: Interval) extends JSONObjectProvider {

  def toJObject: JObject =
      mkType("Position") ~
      ("start", interval.start) ~
      ("end", interval.end)
}

object JSONInterval {
  val singular = "position"
  val plural = "positions"
  val typename = "Position"
}

class JSONProvenance(mention: Mention) extends JSONObjectProvider {

  def toJObject: JObject =
      mkType("Provenance") ~
      (JSONDocument.singular -> id(mention.document)) ~
      (JSONSentence.singular -> id(mention.sentenceObj)) ~
      (JSONInterval.plural -> Seq(mention.tokenInterval).map(new JSONInterval(_).toJObject)) // TODO: We only have one
}

object JSONProvenance {
  val singular = "provenance"
  val plural = "provenances"
  val typename = "Provenance"
}

class JSONMention(mention: Mention) extends JSONObjectProvider {
 
  def toJObject: JObject =
      mkType("Entity") ~
      mkId(mention) ~
      ("labels" -> mention.labels) ~
      ("text" -> mention.text) ~
      (JSONProvenance.singular -> new JSONProvenance(mention).toJObject) ~
      ("rule" -> mention.foundBy)
}

object JSONMention {
  val singular = "entity"
  val plural = "entities"
  val typename = "Entity"
}

class JSONTextBoundMention(mention: TextBoundMention) extends JSONMention(mention) {
  
  override def toJObject: JObject =
     mkType("Entity") ~
     super.toJObject ~
     ("states" -> mention.attachments.map(newJSONAttachment(_).toJObject).toList)
}

class JSONRelationMention(mention: RelationMention) extends JSONMention(mention) {
  
  override def toJObject: JObject = 
    mkType("Undirected Relation") ~ // TODO: Is this true, see no trigger
    super.toJObject
    // TODO: No trigger
    // TODO: The picture is vague
    (JSONNamedArgument.plural, mention.arguments.map(item => new JSONNamedArgument(item._1, item._2).toJObject).toList)
}

class JSONEventMention(mention: EventMention) extends JSONMention(mention) {
  
  override def toJObject: JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil) // TODO: are these correct names?
    val targets = mention.arguments.getOrElse("effect", Nil)
      
    mkType("Directed Relation") ~ // TODO: Is this true?
    super.toJObject ~
    ("trigger" -> newJSONMention(mention.trigger).toJObject) ~ // TODO: Why not just id?
    ("sources" -> sources.map(newJSONMention(_).toJObject)) ~ // TODO: Why not just id?
    ("destinations" -> targets.map(newJSONMention(_).toJObject)) // TODO: Why not just id?
  }
}

class JSONGraphMapPair(key: String, value: Option[DirectedGraph[String]]) extends JSONObjectProvider {
  
  def toJObject: JObject =
    ("dependencies" ->
      ("key" -> key) // TODO: How are these described?
    )
}

class JSONWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONObjectProvider {
  
  def toJObject: JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (optionArray.isDefined) Option(optionArray.get(index))
        else None
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)

    mkType("Word") ~
    ("text" -> (if (text.isDefined) Option(text.get.substring(startOffset, endOffset)) else None)) ~
    ("tag" -> getOrNone(sentence.tags)) ~
    ("entity" -> getOrNone(sentence.entities)) ~
    ("startOffset" -> startOffset) ~
    ("endOffset" -> endOffset) ~
    ("lemma" -> getOrNone(sentence.lemmas)) ~
    ("chunk" -> getOrNone(sentence.chunks))
  }
}

object JSONWord {
  val singular = "word"
  val plural = "words"
  val typename = "Word"
}

class JSONSentence(sentence: Sentence, text: Option[String]) extends JSONObjectProvider {
  
  def toJObject: JObject = {
    val key = "universal-enhanced"
    val dependencies = sentence.graphs.get(key)
    
    mkType("Sentence") ~
    mkId(sentence) ~
    (JSONWord.plural -> sentence.words.take(2).indices.map(new JSONWord(sentence, _, text).toJObject).toList) ~
    ("text" -> sentence.getSentenceText()) // ~
    //new JSONGraphMapPair(key, dependencies).toJObject
  }
}

object JSONSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JSONDocument(document: Document, mentions: Seq[Mention]) extends JSONObjectProvider {
  
  def toJObject: JObject =
    mkContext() ~
    mkType("Document") ~
    (JSONSentence.plural -> document.sentences.map(new JSONSentence(_, document.text).toJObject).toList) // ~
//    (JSONMention.plural -> mentions.map(newJSONMention(_).toJObject).toList)
}

object JSONDocument {
  val singular = "document"
  val plural = "documents"
  val typename = "Document"
}
