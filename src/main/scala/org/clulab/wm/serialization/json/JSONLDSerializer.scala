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
  
  // These are placeholder, no-op implementations
  def toJObject(jsonldPublisher: JSONLDPublisher, asRef: Boolean = false): JsonAST.JObject = new JsonAST.JObject(Nil)
  
  // These are placeholder, no-op implementations
  def toJValue(jsonldPublisher: JSONLDPublisher, asRef: Boolean = false): JsonAST.JValue = JsonAST.JNothing
  
  def newJSONLDMention(mention: Mention): JSONLDMention = {
    val causes = mention.arguments("cause")
    val effects = mention.arguments("effects")
    val others = mention.arguments.keySet.exists(key => key != "cause" && key != "effect")
    
    if (!causes.isEmpty && !effects.isEmpty)
      new JSONLDDirectedRelation(mention)
    else if (others || !causes.isEmpty || !effects.isEmpty)
      new JSONLDUndirectedRelation(mention)
    else
      new JSONLDEntity(mention)
  }      

  def newJSONLDAttachment(attachment: Attachment, mention: Mention): JSONLDAttachment = attachment match {
    // These attachments are the first class in the matching package and could be changed
    case attachment: Quantification => new JSONLDAttachment("QUANT", attachment.quantifier, None, mention: Mention)
    case attachment: Increase => new JSONLDAttachment("INC", attachment.trigger, attachment.quantifier, mention: Mention)
    case attachment: Decrease => new JSONLDAttachment("DEC", attachment.trigger, attachment.quantifier, mention: Mention)
  }
}

object JSONLDObjectProvider {
  case class AnnotatedDocument(var document: Document, var mentions: Seq[Mention])
  type Anthology = Seq[AnnotatedDocument]  
}

class JSONLDNamedArgument(name: String, attachments: Seq[Mention]) extends JSONLDObjectProvider {

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
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

class JSONLDModifier(text: String, mention: Mention) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
      publisher.mkType(JSONLDModifier.singular) ~
      ("text" -> text) ~
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
      ("intercept" -> None) ~
      ("mu" -> None) ~
      ("sigma" -> None)
      // TODO: Figure out how to get these values
  }
}

object JSONLDModifier {
  val singular = "modifier"
  val plural = "modifiers"
  val typename = "Modifier"
}

class JSONLDAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]], mention: Mention) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val mods =
        if (!modifiers.isDefined) None
        else Some(modifiers.get.map(new JSONLDModifier(_, mention).toJObject(publisher)).toList)
    
    publisher.mkType(JSONLDAttachment.singular) ~
        publisher.mkId(this, JSONLDAttachment.singular) ~
        ("type", kind) ~
        ("text", text) ~
        (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
        ("modifiers", mods)
  }
}

object JSONLDAttachment {
  val singular = "state"
  val plural = "states"
  val typename = "State"
}

class JSONLDInterval(interval: Interval, asRef: Boolean = false) extends JSONLDObjectProvider {

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
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
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      publisher.mkType("Provenance") ~
//      publisher.mkRef(JSONLDDocument.singular, mention.document) ~ // So need to know document!
//      publisher.mkRef(JSONLDSentence.singular, mention.sentenceObj) ~
      (JSONLDInterval.plural -> Seq(mention.tokenInterval).map(new JSONLDInterval(_).toJObject(publisher))) // TODO: We only have one
}

// TODO: accept with or without interval in case it is not known
object JSONLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
  val typename = "Provenance"
}

class JSONLDMention(mention: Mention) extends JSONLDObjectProvider {
 
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      ("labels" -> mention.labels) ~
      ("text" -> mention.text) ~
      ("rule" -> mention.foundBy) ~
      ("score" -> None) ~ // Figure out how to look up?, maybe like the sigma
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
      ("states" -> mention.attachments.map(newJSONLDAttachment(_, mention).toJObject(publisher)).toList)
      
    def getTrigger() = mention match {
      case mention: TextBoundMention => None
      case mention: EventMention => Some(mention.trigger)
      case mention: RelationMention => None
    }
}

object JSONLDMention {
  val singular = "entity"
  val plural = "entities"
  val typename = "Entity"
}

class JSONLDAbbreviatedMention(mention: Mention) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      ("text" -> mention.text) ~
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher))
}

class JSONLDEntity(mention: Mention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
     publisher.mkType("Entity") ~
     super.toJObject(publisher) ~
     ("states" -> mention.attachments.map(newJSONLDAttachment(_, mention).toJObject(publisher)).toList)
}

class JSONLDDirectedRelation(mention: Mention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil)
    val targets = mention.arguments.getOrElse("effect", Nil)
    val trigger = getTrigger()
    val triggerMention =
        if (trigger.isDefined) Some(new JSONLDAbbreviatedMention(trigger.get).toJObject(publisher))
        else None
 
    publisher.mkType("DirectedRelation") ~
    publisher.mkId(this, "DirectedRelation") ~
    super.toJObject(publisher) ~
    ("trigger" -> triggerMention) ~
    ("sources" -> sources.map(newJSONLDMention(_).toJObject(publisher, true))) ~ // TODO: Instead, ask publisher for ref to mention
    ("destinations" -> targets.map(newJSONLDMention(_).toJObject(publisher, true)))
  }
}

class JSONLDUndirectedRelation(mention: Mention) extends JSONLDMention(mention) {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val arguments = mention.arguments.values.flatten // The keys are skipped
    val argumentMentions =
        if (arguments.isEmpty) None
        else Some(mention.arguments.map(item => new JSONLDNamedArgument(item._1, item._2).toJObject(publisher)).toList)

    publisher.mkType("UndirectedRelation") ~
    publisher.mkId(this, "UndirectedRelation") ~
    super.toJObject(publisher) ~
    (JSONLDNamedArgument.plural, argumentMentions)
  }
}

class JSONLDEdge(edge: (Int, Int, String), words: Seq[JSONLDWord]) extends JSONLDObjectProvider {

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val source = words(edge._1)
    val destination = words(edge._2)
    
    //publisher.mkType(JSONLDWord.typename) ~
        publisher.mkRef("source", source) ~
        publisher.mkRef("destination", destination) ~
        ("relation" -> edge._3)
  }  
}

class JSONLDGraphMapPair(key: String, directedGraph: DirectedGraph[String], words: Seq[JSONLDWord]) extends JSONLDObjectProvider {
 
  override def toJValue(publisher: JSONLDPublisher, asRef: Boolean = false): JValue = {
    val edges = directedGraph.allEdges.map(new JSONLDEdge(_, words))
    val jsonldEdges = edges.map(_.toJObject(publisher)).toList
    
    new JArray(jsonldEdges) // JSONLDGraphMapPair.plural ->
//      jsonldEdges
//    (JSONLDGraphMapPair.plural ->
//        ("kind" -> key) ~
//    new JObject(jsonldEdges)
//    )
   // jsonldEdges
  }
}

object JSONLDGraphMapPair {
  val singular = "dependency"
  val plural = "dependencies"
  val typename = "Dependency"  
}

class JSONLDWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (optionArray.isDefined) Option(optionArray.get(index))
        else None
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)

    publisher.mkType(JSONLDWord.typename) ~
    publisher.mkId(this, JSONLDWord.typename) ~
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
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val jsonldWords = sentence.words.indices.map(new JSONLDWord(sentence, _, text))
      
    val key = "universal-enhanced"
    val dependencies = sentence.graphs.get(key)
    val jsonldGraphMapPair = // In this case, register the type
        if (dependencies.isDefined) Some(new JSONLDGraphMapPair(key, dependencies.get, jsonldWords).toJValue(publisher))
        else None
          
    publisher.mkType(JSONLDSentence.typename) ~
        publisher.mkId(this, JSONLDSentence.typename) ~
        (JSONLDWord.plural -> jsonldWords.map(_.toJObject(publisher)).toList) ~
        ("text" -> sentence.getSentenceText()) ~
        //jsonldGraphMapPair
        ("dependencies" -> jsonldGraphMapPair) // This is a value, and thus an array
  }
}

object JSONLDSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JSONLDDocument(annotatedDocument: JSONLDObjectProvider.AnnotatedDocument) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val sentences = annotatedDocument.document.sentences.map(new JSONLDSentence(_, annotatedDocument.document.text))
    
      publisher.mkType(JSONLDDocument.typename) ~
      publisher.mkId(this, JSONLDDocument.typename) ~
      (JSONLDSentence.plural -> sentences.map(_.toJObject(publisher)).toList) //~
      //(JSONLDMention.plural -> annotatedDocument.mentions.map(newJSONLDMention(_, this, sentences).toJObject).toList)
  }
}

object JSONLDDocument {
  val singular = "document"
  val plural = "documents"
  val typename = "Document"
}

class JSONLDAnthology(anthology: JSONLDObjectProvider.Anthology) extends JSONLDObjectProvider {
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    publisher.mkType(JSONLDAnthology.typename) ~
    publisher.mkId(this, JSONLDAnthology.typename) ~
    (JSONLDDocument.plural -> anthology.map(annotatedDocument => new JSONLDDocument(annotatedDocument).toJObject(publisher)).toList) ~
    (JSONLDMention.plural -> anthology.flatMap(_.mentions).map(newJSONLDMention(_).toJObject(publisher)).toList)
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

  def mkId(any: JSONLDObjectProvider, typename: String) = ("@id" -> ("_:" + typename + "_" + id(any)))

  def mkId(document: JSONLDDocument, typename: String) = ("@id" -> ("_:" + id(document)))
  
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
    // TODO: Alphabetize these
    def mkContext(name: String): JsonAST.JField = new JsonAST.JField(name, JSONLDPublisher.home + "#user-content-" + name.toLowerCase())
    
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
  val home = "https://github.com/clulab/eidos/wiki/JSON-LD"
}