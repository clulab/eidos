package org.clulab.wm.serialization.json

import scala.collection.mutable

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

  def toJFields(jsonldPublisher: JSONLDPublisher): List[JsonAST.JField] = List(new JsonAST.JField("nothing", JsonAST.JNothing))
  
  def toJObject(jsonldPublisher: JSONLDPublisher): JObject
  
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
}

class JSONLDNamedArgument(name: String, attachments: Seq[Mention]) extends JSONLDObjectProvider {

  def toJObject(publisher: JSONLDPublisher): JObject =
    (JSONLDNamedArgument.singular ->
        publisher.mkType("Argument") ~
        ("name" -> name) ~
        ("attachments" -> attachments.map(newJSONLDMention(_).toJObject(publisher))) // TODO: Why not id?
    )
}

object JSONLDNamedArgument {
  val singular = "argument"
  val plural = "arguments"
  val typename = "NamedArgument"
}

class JSONLDAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]]) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject =
      publisher.mkType(kind) ~
          ("text", text) ~
          ("modifiers", modifiers) // TODO: This is as far as it goes
}

class JSONLDInterval(interval: Interval) extends JSONLDObjectProvider {

  def toJObject(publisher: JSONLDPublisher): JObject =
      publisher.mkType("Position") ~
          ("start", interval.start) ~
          ("end", interval.end)
}

object JSONLDInterval {
  val singular = "position"
  val plural = "positions"
  val typename = "Position"
}

class JSONLDProvenance(mention: Mention) extends JSONLDObjectProvider {
  // Need to know sentences and documents
  
  def toJObject(publisher: JSONLDPublisher): JObject =
      publisher.mkType("Provenance") ~
//      publisher.mkRef(JSONLDDocument.singular, mention.document) ~ // So need to know document!
//      publisher.mkRef(JSONLDSentence.singular, mention.sentenceObj) ~
      (JSONLDInterval.plural -> Seq(mention.tokenInterval).map(new JSONLDInterval(_).toJObject(publisher))) // TODO: We only have one
}

object JSONLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
  val typename = "Provenance"
}

class JSONLDMention(mention: Mention) extends JSONLDObjectProvider {
 
  def toJObject(publisher: JSONLDPublisher): JObject =
      publisher.mkType("Entity") ~
      publisher.mkId(this) ~
      ("labels" -> mention.labels) ~
      ("text" -> mention.text) ~
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
      ("rule" -> mention.foundBy)
}

object JSONLDMention {
  val singular = "entity"
  val plural = "entities"
  val typename = "Entity"
}

class JSONLDTextBoundMention(mention: TextBoundMention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher): JObject =
     publisher.mkType("Entity") ~
     super.toJObject(publisher) ~
     ("states" -> mention.attachments.map(newJSONLDAttachment(_).toJObject(publisher)).toList)
}

class JSONLDRelationMention(mention: RelationMention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher): JObject = 
    publisher.mkType("Undirected Relation") ~ // TODO: Is this true, see no trigger
    super.toJObject(publisher) ~
    // TODO: No trigger
    // TODO: The picture is vague
    (JSONLDNamedArgument.plural, mention.arguments.map(item => new JSONLDNamedArgument(item._1, item._2).toJObject(publisher)).toList)
}

class JSONLDEventMention(mention: EventMention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher): JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil) // TODO: are these correct names?
    val targets = mention.arguments.getOrElse("effect", Nil)
      
    publisher.mkType("Directed Relation") ~ // TODO: Is this true?
    super.toJObject(publisher) ~
    ("trigger" -> newJSONLDMention(mention.trigger).toJObject(publisher)) ~ // TODO: Why not just id?
    ("sources" -> sources.map(newJSONLDMention(_).toJObject(publisher))) ~ // TODO: Why not just id?
    ("destinations" -> targets.map(newJSONLDMention(_).toJObject(publisher))) // TODO: Why not just id?
  }
}

class JSONLDEdge(edge: (Int, Int, String), words: Seq[JSONLDWord]) extends JSONLDObjectProvider {

  def toJObject(publisher: JSONLDPublisher): JObject = {
    val source = words(edge._1)
    val destination = words(edge._2)
    
    publisher.mkType(JSONLDWord.typename) ~
        publisher.mkRef("source", source) ~
        publisher.mkRef("destination", destination) ~
        ("relation" -> edge._3)
  }  
}

class JSONLDGraphMapPair(key: String, directedGraph: DirectedGraph[String], words: Seq[JSONLDWord]) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject = {
    val edges = directedGraph.allEdges.map(new JSONLDEdge(_, words))
    
    (JSONLDGraphMapPair.plural ->
        ("kind" -> key) ~
        (JSONLDGraphMapPair.plural -> edges.map(_.toJObject(publisher)).toList)
    )
  }
}

object JSONLDGraphMapPair {
  val singular = "dependency"
  val plural = "dependencies"
  val typename = "Dependency"  
}

class JSONLDWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (optionArray.isDefined) Option(optionArray.get(index))
        else None
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)

    (JSONLDWord.singular ->
        publisher.mkType(JSONLDWord.typename) ~
        publisher.mkId(this) ~
        ("text" -> (if (text.isDefined) Option(text.get.substring(startOffset, endOffset)) else None)) ~
        ("tag" -> getOrNone(sentence.tags)) ~
        ("entity" -> getOrNone(sentence.entities)) ~
        ("startOffset" -> startOffset) ~
        ("endOffset" -> endOffset) ~
        ("lemma" -> getOrNone(sentence.lemmas)) ~
        ("chunk" -> getOrNone(sentence.chunks))
    )
  }
}

object JSONLDWord {
  val singular = "word"
  val plural = "words"
  val typename = "Word"
}

class JSONLDSentence(sentence: Sentence, text: Option[String]) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject = {
    val jsonldWords = sentence.words.indices.map(new JSONLDWord(sentence, _, text))
      
    val key = "universal-enhanced"
    val dependencies = sentence.graphs.get(key)
    val jsonldGraphMapPair = // In this case, register the type
        if (dependencies.isDefined) Some(new JSONLDGraphMapPair(key, dependencies.get, jsonldWords).toJObject(publisher))
        else None
          
    (JSONLDSentence.singular ->
        publisher.mkType(JSONLDSentence.typename) ~
        publisher.mkId(this) ~
        (JSONLDWord.plural -> jsonldWords.map(_.toJObject(publisher)).toList) ~
        ("text" -> sentence.getSentenceText()) ~
        ("dependencies" -> jsonldGraphMapPair)
    )
  }
}

object JSONLDSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JSONLDDocument(annotatedDocument: JSONLDObjectProvider.AnnotatedDocument) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject =
      publisher.mkType(JSONLDDocument.typename) ~
      publisher.mkId(this) ~
      (JSONLDSentence.plural -> annotatedDocument.document.sentences.map(new JSONLDSentence(_, annotatedDocument.document.text).toJObject(publisher)).toList) // ~
//      (JSONLDMention.plural -> mentions.map(newJSONLDMention(_).toJObject).toList)
}

object JSONLDDocument {
  val singular = "document"
  val plural = "documents"
  val typename = "Document"
}

class JSONLDAnthology(anthology: JSONLDObjectProvider.Anthology) extends JSONLDObjectProvider {
  
  def toJObject(publisher: JSONLDPublisher): JObject = {
    publisher.mkType(JSONLDAnthology.typename) ~
    publisher.mkId(this) ~
    (JSONLDDocument.plural -> anthology.map(annotatedDocument => new JSONLDDocument(annotatedDocument).toJObject(publisher)).toList)
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
  val typenames = mutable.HashSet[String]()
  
  // TODO: Make these nicer by handing out ids from somewhere
  def id(any: Any): String = System.identityHashCode(any).toString
  
  // TODO: Documents may already have some kind of ID.  Should it be used?
  def id(document: Document): String = document.id.getOrElse(id(document.asInstanceOf[Any]))

  def mkId(any: JSONLDObjectProvider) = ("@id" -> (JSONLDPublisher.home + "id/" + id(any)))

  def mkId(document: JSONLDDocument) = ("@id" -> id(document))
  
  def mkType(typename: String) = {
    typenames += typename
    ("@type" -> typename)
  }
  
  def mkTypeField(typename: String): JsonAST.JField = {
    typenames += typename
    
    new JField("@type", typename)
  }
  
//  def mkContext(): JsonAST.JField = {
//    def mkContext(name: String): JsonAST.JField = new JsonAST.JField(name, JSONLDPublisher.home + name)
//    
//    new JsonAST.JField("@context", new JObject(typenames.map(mkContext(_)).toList))
//  }

  def mkContext(): JObject = {
    def mkContext(name: String): JsonAST.JField = new JsonAST.JField(name, JSONLDPublisher.home + name)
    
    new JObject(typenames.map(mkContext(_)).toList)
  }
  
  def mkRef(name: String, jsonldObjectProvider: JSONLDObjectProvider): JsonAST.JField = {
    val ref = "_:5"
    
    new JsonAST.JField(name, new JObject(List(new JField("@id", ref))))
  }
  
  def register(jsonLDOjectProvider: JSONLDObjectProvider): String = {
    // gives access to contents which are used to generate IDs
    // also provides typename for use in generating context
    // value, typename, 
    "hello"
  }
  
  def publish(): JValue = {
    val jObject = jsonldObjectProvider.toJObject(this) // turn into list of fields
    val context = mkContext 
    val itsValues = jObject.values
    
    val result = ("@context" -> context) ~
    jObject
    
//    val fields = List(
//        context
//        // the rest of the object
//    )
    
    
    // Is a field
    
    
    // Look over the jObject and construct the context?
    
//    mkContext() ~
    // Add the context on top
    // Reset the id map

   // jObject
//    new JObject(fields)
    result
  }  
}

object JSONLDPublisher {
  val home = "http://www.example.com/"
}