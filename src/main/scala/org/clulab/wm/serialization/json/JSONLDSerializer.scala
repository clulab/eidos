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

abstract class JSONLDObjectProvider(val value: Any) {

  def toJObject(jsonldPublisher: JSONLDPublisher): JObject
  
  // TODO: Make these nicer by handing out ids from somewhere
  def id(any: Any): String = System.identityHashCode(any).toString
  
  // TODO: Documents may already have some kind of ID.  Should it be used?
  def id(document: Document): String = document.id.getOrElse(id(document.asInstanceOf[Any]))

  def mkId(any: Any) = ("@id" -> (JSONLDObjectProvider.home + "id/" + id(any)))

  def mkId(document: Document) = ("@id" -> id(document))
  
  def mkType(value: String) = ("@type" -> value)
  
  def newJSONLDMention(mention: Mention): JSONLDMention = mention match {
    case mention: TextBoundMention => new JSONLDTextBoundMention(mention)
    case mention: RelationMention => new JSONLDRelationMention(mention)
    case mention: EventMention => new JSONLDEventMention(mention)
  }      

  def newJSONLDAttachment(attachment: Attachment): JSONLDAttachment = attachment match {
    // These attachments are the first class in the matching package and could be changed
    case attachment: Quantification => new JSONLDAttachment("QUANT", attachment.quantifier, None)
    case attachment: Increase => new JSONLDAttachment("INC", attachment.trigger, attachment.quantifier)
    case attachment: Decrease => new JSONLDAttachment("DEC", attachment.trigger, attachment.quantifier)
  }
}

object JSONLDObjectProvider {
  case class AnnotatedDocument(var document: Document, var mentions: Seq[Mention])
  type Anthology = Seq[AnnotatedDocument]  
  
  val home = "http://www.example.com/"
}

class JSONLDNamedArgument(name: String, attachments: Seq[Mention]) extends JSONLDObjectProvider {

  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
      mkType("Argument") ~
      ("name" -> name) ~
      ("attachments" -> attachments.map(newJSONLDMention(_).toJObject(jsonldPublisher))) // TODO: Why not id?
}

object JSONLDNamedArgument {
  val singular = "argument"
  val plural = "arguments"
  val typename = "NamedArgument"
}

class JSONLDAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]]) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
      mkType(kind) ~
      ("text", text) ~
      ("modifiers", modifiers) // TODO: This is as far as it goes
}

class JSONLDInterval(interval: Interval) extends JSONLDObjectProvider {

  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
      mkType("Position") ~
      ("start", interval.start) ~
      ("end", interval.end)
}

object JSONLDInterval {
  val singular = "position"
  val plural = "positions"
  val typename = "Position"
}

class JSONLDProvenance(mention: Mention) extends JSONLDObjectProvider {

  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
      mkType("Provenance") ~
      (JSONLDDocument.singular -> id(mention.document)) ~
      (JSONLDSentence.singular -> id(mention.sentenceObj)) ~
      (JSONLDInterval.plural -> Seq(mention.tokenInterval).map(new JSONLDInterval(_).toJObject(jsonldPublisher))) // TODO: We only have one
}

object JSONLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
  val typename = "Provenance"
}

class JSONLDMention(mention: Mention) extends JSONLDObjectProvider {
 
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
      mkType("Entity") ~
      mkId(mention) ~
      ("labels" -> mention.labels) ~
      ("text" -> mention.text) ~
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(jsonldPublisher)) ~
      ("rule" -> mention.foundBy)
}

object JSONLDMention {
  val singular = "entity"
  val plural = "entities"
  val typename = "Entity"
}

class JSONLDTextBoundMention(mention: TextBoundMention) extends JSONLDMention(mention) {
  
  override def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
     mkType("Entity") ~
     super.toJObject(jsonldPublisher) ~
     ("states" -> mention.attachments.map(newJSONLDAttachment(_).toJObject(jsonldPublisher)).toList)
}

class JSONLDRelationMention(mention: RelationMention) extends JSONLDMention(mention) {
  
  override def toJObject(jsonldPublisher: JSONLDPublisher): JObject = 
    mkType("Undirected Relation") ~ // TODO: Is this true, see no trigger
    super.toJObject(jsonldPublisher) ~
    // TODO: No trigger
    // TODO: The picture is vague
    (JSONLDNamedArgument.plural, mention.arguments.map(item => new JSONLDNamedArgument(item._1, item._2).toJObject(jsonldPublisher)).toList)
}

class JSONLDEventMention(mention: EventMention) extends JSONLDMention(mention) {
  
  override def toJObject(jsonldPublisher: JSONLDPublisher): JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil) // TODO: are these correct names?
    val targets = mention.arguments.getOrElse("effect", Nil)
      
    mkType("Directed Relation") ~ // TODO: Is this true?
    super.toJObject(jsonldPublisher) ~
    ("trigger" -> newJSONLDMention(mention.trigger).toJObject(jsonldPublisher)) ~ // TODO: Why not just id?
    ("sources" -> sources.map(newJSONLDMention(_).toJObject(jsonldPublisher))) ~ // TODO: Why not just id?
    ("destinations" -> targets.map(newJSONLDMention(_).toJObject(jsonldPublisher))) // TODO: Why not just id?
  }
}

class JSONLDGraphMapPair(key: String, value: Option[DirectedGraph[String]]) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
    ("dependencies" ->
      ("key" -> key) // TODO: How are these described?
    )
}

class JSONLDWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject = {
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

object JSONLDWord {
  val singular = "word"
  val plural = "words"
  val typename = "Word"
}

class JSONLDSentence(sentence: Sentence, text: Option[String]) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject = {
    val key = "universal-enhanced"
    val dependencies = sentence.graphs.get(key)
    
    mkType("Sentence") ~
    mkId(sentence) ~
    (JSONLDWord.plural -> sentence.words.take(2).indices.map(new JSONLDWord(sentence, _, text).toJObject(jsonldPublisher)).toList) ~
    ("text" -> sentence.getSentenceText()) // ~
    //new JSONLDGraphMapPair(key, dependencies).toJObject
  }
}

object JSONLDSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JSONLDDocument(annotatedDocument: JSONLDObjectProvider.AnnotatedDocument) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject =
    mkType(JSONLDDocument.singular) ~
    (JSONLDSentence.plural -> annotatedDocument.document.sentences.map(new JSONLDSentence(_, annotatedDocument.document.text).toJObject(jsonldPublisher)).toList) // ~
//    (JSONLDMention.plural -> mentions.map(newJSONLDMention(_).toJObject).toList)
}

object JSONLDDocument {
  val singular = "document"
  val plural = "documents"
  val typename = "Document"
}

class JSONLDAnthology(anthology: JSONLDObjectProvider.Anthology) extends JSONLDObjectProvider {
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject = {
    mkType(JSONLDAnthology.singular) ~
    (JSONLDDocument.plural -> anthology.map(annotatedDocument => new JSONLDDocument(annotatedDocument).toJObject(jsonldPublisher)).toList) // to document
    
    // Maybe have different entity collection, because they could span documents and sentences?
    // then do entities next
  }
}

object JSONLDAnthology {
  val singular = "anthology"
  val plural = "anthologies"
  val typename = "Anthology"
}


class JSONLDPublisher(jsonldObjectProvider: JSONLDObjectProvider) {
  
  def mkContext():JObject = {
    def mkContext(name: String) = (name -> (JSONLDObjectProvider.home + name))
    // Depends on what kind of objects have been included.
    ("@context" ->
        mkContext(JSONLDDocument.typename) ~
        mkContext(JSONLDSentence.typename) ~
        mkContext(JSONLDWord.typename) ~
        mkContext(JSONLDMention.typename) ~
        mkContext(JSONLDProvenance.typename) ~
        mkContext(JSONLDInterval.typename) ~
        mkContext(JSONLDNamedArgument.typename) 
    )
  }
  
  def register(jsonLDOjectProvider: JSONLDObjectProvider): String = {
    // gives access to contents which are used to generate IDs
    // also provides typename for use in generating context
    // value, typename, 
    "hello"
  }
  
  def publish(): JValue = {
    val jObject = jsonldObjectProvider.toJObject(this)
    
//    mkContext() ~
    // Add the context on top
    // Reset the id map

    jObject
  }
}