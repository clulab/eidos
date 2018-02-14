package org.clulab.wm.serialization.json

import scala.collection.mutable

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.odin.RelationMention
import org.clulab.odin.EventMention

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Interval
import org.clulab.wm.Aliases.Quantifier
import org.clulab.wm.{Quantification, Increase, Decrease}

import org.json4s.JsonAST._
import org.json4s.JsonDSL._ // ~ comes from here
import org.json4s.jackson.JsonMethods._


// This is an object than when asked to convert itself a JSON object or value, converts
// itself in a way that conforms to the JSON-LD standard as well.
abstract class JSONLDObject(val value: Any) {

  def typename: String
  
  // This is for individual objects
  def toJObject(jsonldPublisher: JSONLDPublisher, asRef: Boolean = false): JObject = ???
  
  // This is for arrays of values
  def toJValue(jsonldPublisher: JSONLDPublisher, asRef: Boolean = false): JValue = ???
  
  // TODO: This should use the yaml file to distinguish
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
    case attachment: Quantification => new JSONLDAttachment("QUANT", attachment.quantifier, None, mention: Mention)
    case attachment: Increase => new JSONLDAttachment("INC", attachment.trigger, attachment.quantifier, mention: Mention)
    case attachment: Decrease => new JSONLDAttachment("DEC", attachment.trigger, attachment.quantifier, mention: Mention)
  }
}

object JSONLDObject {
  case class AnnotatedDocument(var document: Document, var mentions: Seq[Mention])
  type Anthology = Seq[AnnotatedDocument]  
}

// This class helps publish/convert a JSONLDObject to JSONLD by keeping track of
// what types are included and providing IDs so that references to can be made
// within the JSON structure.
class JSONLDPublisher(jsonldObjectProvider: JSONLDObject) {
  val typenames = mutable.HashSet[String]()
  
  def id(jsonldObject: JSONLDObject): String = System.identityHashCode(jsonldObject.value).toString

  def mkId(jsonldObject: JSONLDObject) = ("@id" -> ("_:" + jsonldObject.typename + "_" + id(jsonldObject)))
  
  def mkType(typename: String): (String, String) = {
    typenames += typename
    ("@type" -> typename)
  }
  
  def mkType(jsonldObject: JSONLDObject): (String, String) = mkType(jsonldObject.typename)

  def mkContext(): JObject = {
    def mkContext(name: String): JField = new JField(name, JSONLDPublisher.home + "#user-content-" + name.toLowerCase())
    
    new JObject(typenames.toList.sorted.map(mkContext(_)))
  }
  
  def mkRef(name: String, jsonldObjectProvider: JSONLDObject): JField = {
    val ref = "_:5"
    
    new JField(name, new JObject(List(new JField("@id", ref))))
  }
  
  def publish(): JValue =
      ("@context" -> mkContext) ~
      jsonldObjectProvider.toJObject(this)
}

object JSONLDPublisher {
  val home = "https://github.com/clulab/eidos/wiki/JSON-LD"
}

class JSONLDArgument(mention: Mention) extends JSONLDObject {
  
  def typename = "Argument"

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject=
      publisher.mkType(this) ~
          newJSONLDMention(mention).toJObject(publisher, true) // TODO: Instead, publisher.asRef(mention)
}

object JSONLDArgument {
  val singular = "argument"
  val plural = "arguments"
}

class JSONLDModifier(text: String, mention: Mention) extends JSONLDObject {

  def typename = "Modifier"

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
      publisher.mkType(this) ~
          ("text" -> text) ~
          (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
          // TODO: Figure out how to get these values
          ("intercept" -> None) ~
          ("mu" -> None) ~
          ("sigma" -> None)
  }
}

object JSONLDModifier {
  val singular = "modifier"
  val plural = "modifiers"
}

class JSONLDAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]], mention: Mention) extends JSONLDObject {
  
  def typename = "State"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val mods =
        if (!modifiers.isDefined) None
        else Some(modifiers.get.map(new JSONLDModifier(_, mention).toJObject(publisher)).toList)
    
    publisher.mkType(this) ~
        publisher.mkId(this) ~
        ("type", kind) ~
        ("text", text) ~
        (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
        (JSONLDModifier.plural -> mods)
  }
}

object JSONLDAttachment {
  val singular = "state"
  val plural = "states"
}

class JSONLDInterval(interval: Interval, asRef: Boolean = false) extends JSONLDObject {

  def typename = "Position"

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      publisher.mkType(this) ~
          ("start", interval.start) ~
          ("end", interval.end)
}

object JSONLDInterval {
  val singular = "position"
  val plural = "positions"
}

// TODO Decide for which ones position is pertinent
// Get the document and sentence
class JSONLDProvenance(mention: Mention, skipPositions: Boolean = false) extends JSONLDObject {
  
  def typename = "Provenance"

  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val positions =
        if (skipPositions) None
        else Some(Seq(mention.tokenInterval).map(new JSONLDInterval(_).toJObject(publisher)))
            
      publisher.mkType(this) ~
//      publisher.mkRef(JSONLDDocument.singular, mention.document) ~ // So need to know document!
//      publisher.mkRef(JSONLDSentence.singular, mention.sentenceObj) ~
      (JSONLDInterval.plural -> positions)
  }
}

object JSONLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
}

abstract class JSONLDMention(mention: Mention) extends JSONLDObject {
 
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      publisher.mkType(this) ~ // TODO: Can this be left out?
          publisher.mkId(this) ~
          ("labels" -> mention.labels) ~
          ("text" -> mention.text) ~
          ("rule" -> mention.foundBy) ~
          ("score" -> None) ~ // Figure out how to look up?, maybe like the sigma
          (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher)) ~
          (JSONLDAttachment.plural -> mention.attachments.map(newJSONLDAttachment(_, mention).toJObject(publisher)).toList)
      
    def getTrigger() = mention match {
      case mention: TextBoundMention => None
      case mention: EventMention => Some(mention.trigger)
      case mention: RelationMention => None
    }
}

object JSONLDMention {
  val singular = "Entity"
  val plural = "entities"
}

class JSONLDTriggerMention(mention: Mention) extends JSONLDObject {
  
  def typename = "Trigger"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
      ("text" -> mention.text) ~
      (JSONLDProvenance.singular -> new JSONLDProvenance(mention).toJObject(publisher))
}

object JSONLDTriggerMention {
  val singular = "trigger"
  val plural = "triggers"
}

class JSONLDEntity(mention: Mention) extends JSONLDMention(mention) {
  
  def typename = "Entity"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject =
     publisher.mkType(this) ~ // TODO: Can this be left out?  See if get doubled
     super.toJObject(publisher)
}

class JSONLDDirectedRelation(mention: Mention) extends JSONLDMention(mention) {
  
  def typename = "DirectedRelation"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil)
    val targets = mention.arguments.getOrElse("effect", Nil)
    val trigger = getTrigger()
    val triggerMention =
        if (trigger.isDefined) Some(new JSONLDTriggerMention(trigger.get).toJObject(publisher))
        else None
 
    super.toJObject(publisher) ~
        (JSONLDTriggerMention.singular -> triggerMention) ~
        ("sources" -> sources.map(newJSONLDMention(_).toJObject(publisher, true))) ~ // TODO: Instead, ask publisher for ref to mention
        ("destinations" -> targets.map(newJSONLDMention(_).toJObject(publisher, true)))
  }
}

class JSONLDUndirectedRelation(mention: Mention) extends JSONLDMention(mention) {
  
  def typename = "UndirectedRelation"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val arguments = mention.arguments.values.flatten // The keys are skipped
    val argumentMentions =
        if (arguments.isEmpty) None
        else Some(arguments.map(new JSONLDArgument(_).toJObject(publisher)).toList)
    val trigger = getTrigger()
    val triggerMention =
        if (trigger.isDefined) Some(new JSONLDTriggerMention(trigger.get).toJObject(publisher))
        else None

    super.toJObject(publisher) ~
        (JSONLDTriggerMention.singular -> triggerMention) ~
        (JSONLDArgument.plural, argumentMentions)
  }
}

// kwa: stopped here
class JSONLDEdge(edge: (Int, Int, String), words: Seq[JSONLDWord]) extends JSONLDObject {

  def typename = "Edge" // TODO: Find a better name
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val source = words(edge._1)
    val destination = words(edge._2)
    
    //publisher.mkType(JSONLDWord.typename) ~
        publisher.mkRef("source", source) ~
        publisher.mkRef("destination", destination) ~
        ("relation" -> edge._3)
  }  
}

class JSONLDGraphMapPair(key: String, directedGraph: DirectedGraph[String], words: Seq[JSONLDWord]) extends JSONLDObject {
 
  def typename = "Dependency"
  
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
}

class JSONLDWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONLDObject {
  
  def typename = "Word"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (optionArray.isDefined) Option(optionArray.get(index))
        else None
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)

    publisher.mkType(this) ~
    publisher.mkId(this) ~
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
}

class JSONLDSentence(sentence: Sentence, text: Option[String]) extends JSONLDObject {
  
  def typename = "Sentence"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val jsonldWords = sentence.words.indices.map(new JSONLDWord(sentence, _, text))
      
    val key = "universal-enhanced"
    val dependencies = sentence.graphs.get(key)
    val jsonldGraphMapPair = // In this case, register the type
        if (dependencies.isDefined) Some(new JSONLDGraphMapPair(key, dependencies.get, jsonldWords).toJValue(publisher))
        else None
          
    publisher.mkType(this) ~
        publisher.mkId(this) ~
        (JSONLDWord.plural -> jsonldWords.map(_.toJObject(publisher)).toList) ~
        ("text" -> sentence.getSentenceText()) ~
        //jsonldGraphMapPair
        ("dependencies" -> jsonldGraphMapPair) // This is a value, and thus an array
  }
}

object JSONLDSentence {
  val singular = "sentence"
  val plural = "sentences"
}

class JSONLDDocument(annotatedDocument: JSONLDObject.AnnotatedDocument) extends JSONLDObject {
  
  def typename = "Document"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    val sentences = annotatedDocument.document.sentences.map(new JSONLDSentence(_, annotatedDocument.document.text))
    
      publisher.mkType(this) ~
      publisher.mkId(this) ~
      (JSONLDSentence.plural -> sentences.map(_.toJObject(publisher)).toList) //~
      //(JSONLDMention.plural -> annotatedDocument.mentions.map(newJSONLDMention(_, this, sentences).toJObject).toList)
  }
}

object JSONLDDocument {
  val singular = "document"
  val plural = "documents"
}

class JSONLDAnthology(anthology: JSONLDObject.Anthology) extends JSONLDObject {
  
  def typename = "Anthology"
  
  override def toJObject(publisher: JSONLDPublisher, asRef: Boolean = false): JObject = {
    publisher.mkType(this) ~
    publisher.mkId(this) ~
    (JSONLDDocument.plural -> anthology.map(annotatedDocument => new JSONLDDocument(annotatedDocument).toJObject(publisher)).toList) ~
    (JSONLDMention.plural -> anthology.flatMap(_.mentions).map(newJSONLDMention(_).toJObject(publisher)).toList)
  }
}

object JSONLDAnthology {
  val singular = "anthology"
  val plural = "anthologies"
}
