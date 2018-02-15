package org.clulab.wm.serialization.json

import java.util.IdentityHashMap

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
abstract class JLDObject(val value: Any) {

  def this() = this(new Object())
  
  def typename: String
  
  // This is for individual objects
  def toJObject(jldPublisher: JLDPublisher, asRef: Boolean = false): JObject = ???
  
  // This is for arrays of values
  def toJValue(jldPublisher: JLDPublisher, asRef: Boolean = false): JValue = ???
  
  def toJObjects(jldObjects: Seq[JLDObject], publisher: JLDPublisher): List[JObject] =
      jldObjects.map(_.toJObject(publisher)).toList
  
  // TODO: This should use the yaml file to distinguish
  def newJLDExtraction(mention: Mention): JLDExtraction = {
    val causes = mention.arguments.getOrElse("cause", Seq())
    val effects = mention.arguments.getOrElse("effects", Seq())
    val others = mention.arguments.keySet.exists(key => key != "cause" && key != "effect")
    
    if (!causes.isEmpty && !effects.isEmpty)
      new JLDDirectedRelation(mention)
    else if (others || !causes.isEmpty || !effects.isEmpty)
      new JLDUndirectedRelation(mention)
    else
      new JLDEntity(mention)
  }      

  def newJLDAttachment(attachment: Attachment, mention: Mention): JLDAttachment = attachment match {
    case attachment: Quantification => new JLDAttachment("QUANT", attachment.quantifier, None, mention: Mention)
    case attachment: Increase => new JLDAttachment("INC", attachment.trigger, attachment.quantifier, mention: Mention)
    case attachment: Decrease => new JLDAttachment("DEC", attachment.trigger, attachment.quantifier, mention: Mention)
  }
}

object JLDObject {
  case class AnnotatedDocument(var document: Document, var mentions: Seq[Mention])
  type Anthology = Seq[AnnotatedDocument]  
}

// This class helps publish/convert a JLDObject to JLD by keeping track of
// what types are included and providing IDs so that references to can be made
// within the JSON structure.
class JLDPublisher(jldObjectProvider: JLDObject) {
  protected val typenames = mutable.HashSet[String]()
  protected val typenamesByIdentity = new IdentityHashMap[Any, String]()
  protected val idsByTypenamesByIdentity: mutable.HashMap[String, IdentityHashMap[Any, Int]] = mutable.HashMap()
  
  protected def mkId(typename: String, id: Int): (String, String) =
      ("@id" -> ("_:" + typename + "_" + id))
    
  def mkId(jldObject: JLDObject): (String, String) = {
    val identity = jldObject.value
    val typename = jldObject.typename
    
    typenamesByIdentity.put(identity, typename) // So that know which idsByTypenamesByIdentity to look in
    val idsByIdentity = idsByTypenamesByIdentity.getOrElseUpdate(typename, new IdentityHashMap[Any, Int]())
    val id = 
        if (idsByIdentity.containsKey(identity))
          idsByIdentity.get(identity)
        else {
          val newId = idsByIdentity.size() + 1
          
          idsByIdentity.put(identity, newId)
          newId
        }
    
    mkId(typename, id)
  }
  
  protected def mkType(typename: String): (String, String) = {
    typenames += typename
    ("@type" -> typename)
  }
  
  def mkType(jldObject: JLDObject): (String, String) = mkType(jldObject.typename)

  def mkContext(): JObject = {
    def mkContext(name: String): JField = new JField(name, "#user-content-" + name.toLowerCase())
    
    val base = new JField("@base", JLDPublisher.base)
    val types = typenames.toList.sorted.map(mkContext(_))
    
    new JObject(base +: types)
  }
  
  def mkRef(name: String, identity: Any): JObject = {
    val typename = typenamesByIdentity.get(identity)
    if (typename == null)
      throw new Exception("Cannot make reference named " + name + " to " + identity)
    
    val id = idsByTypenamesByIdentity(typename).get(identity)
    
    (name ->
        mkId(typename, id)
    )
  }
  
  protected def clear() = {
    typenames.clear()
    typenamesByIdentity.clear()
    idsByTypenamesByIdentity.clear()
  }
  
  def publish(): JValue = {
    clear()
    val jObject = jldObjectProvider.toJObject(this)
      
    ("@context" -> mkContext) ~
        jObject
  }
}

object JLDPublisher {
  val base = "https://github.com/clulab/eidos/wiki/JSON-LD"
}

class JLDArgument(mention: Mention) extends JLDObject(mention) {
  
  def typename = "Argument"

  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject=
      publisher.mkType(this) ~
          newJLDExtraction(mention).toJObject(publisher, true) // TODO: Instead, publisher.asRef(mention)
}

object JLDArgument {
  val singular = "argument"
  val plural = "arguments"
}

class JLDModifier(text: String, mention: Mention) extends JLDObject {

  def typename = "Modifier"

  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
      publisher.mkType(this) ~
          ("text" -> text) ~
          (JLDProvenance.singular -> new JLDProvenance(mention).toJObject(publisher)) ~
          // TODO: Figure out how to get these values
          ("intercept" -> None) ~
          ("mu" -> None) ~
          ("sigma" -> None)
  }
}

object JLDModifier {
  val singular = "modifier"
  val plural = "modifiers"
}

class JLDAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]], mention: Mention) extends JLDObject {
  
  def typename = "State"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val mods =
        if (!modifiers.isDefined) None
        else Some(modifiers.get.map(new JLDModifier(_, mention).toJObject(publisher)).toList)
    
    publisher.mkType(this) ~
        publisher.mkId(this) ~
        ("type", kind) ~
        ("text", text) ~
        (JLDProvenance.singular -> new JLDProvenance(mention).toJObject(publisher)) ~
        (JLDModifier.plural -> mods)
  }
}

object JLDAttachment {
  val singular = "state"
  val plural = "states"
}

class JLDInterval(interval: Interval, asRef: Boolean = false) extends JLDObject {

  def typename = "Position"

  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject =
      publisher.mkType(this) ~
          ("start", interval.start) ~
          ("end", interval.end)
}

object JLDInterval {
  val singular = "position"
  val plural = "positions"
}

class JLDProvenance(mention: Mention, skipPositions: Boolean = false) extends JLDObject(mention) {
  
  def typename = "Provenance"

  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val positions =
        if (skipPositions) None
        else Some(Seq(mention.tokenInterval).map(new JLDInterval(_).toJObject(publisher)))
            
      publisher.mkType(this) ~ // TODO: get these references
//      publisher.mkRef(JLDDocument.singular, mention.document) ~ // So need to know document!
//      publisher.mkRef(JLDSentence.singular, mention.sentenceObj) ~
      (JLDInterval.plural -> positions)
  }
}

object JLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
}

class JLDTrigger(mention: Mention) extends JLDObject(mention) {
  
  def typename = "Trigger"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject =
      ("text" -> mention.text) ~
          (JLDProvenance.singular -> new JLDProvenance(mention).toJObject(publisher))
}

object JLDTrigger {
  val singular = "trigger"
  val plural = "triggers"
}

abstract class JLDExtraction(mention: Mention) extends JLDObject(mention) {
 
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val jldAttachments = mention.attachments.map(newJLDAttachment(_, mention)).toList
    
    publisher.mkType(this) ~
        publisher.mkId(this) ~
        ("labels" -> mention.labels) ~
        ("text" -> mention.text) ~
        ("rule" -> mention.foundBy) ~
        ("score" -> None) ~ // Figure out how to look up?, maybe like the sigma
        (JLDProvenance.singular -> new JLDProvenance(mention).toJObject(publisher)) ~
        (JLDAttachment.plural -> toJObjects(jldAttachments, publisher))
  }
  
    def getTrigger() = mention match {
      case mention: TextBoundMention => None
      case mention: EventMention => Some(mention.trigger)
      case mention: RelationMention => None
    }
}

object JLDExtraction {
  val singular = "extraction"
  val plural = "extractions"
}

class JLDEntity(mention: Mention) extends JLDExtraction(mention) {
  
  def typename = "Entity"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject =
     super.toJObject(publisher)
}

class JLDDirectedRelation(mention: Mention) extends JLDExtraction(mention) {
  
  def typename = "DirectedRelation"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val sources = mention.arguments.getOrElse("cause", Nil)
    val targets = mention.arguments.getOrElse("effect", Nil)
    val trigger = getTrigger()
    val triggerMention =
        if (!trigger.isDefined) None
        else Some(new JLDTrigger(trigger.get).toJObject(publisher))
 
    super.toJObject(publisher) ~
        (JLDTrigger.singular -> triggerMention) ~ // TODO: refs
        ("sources" -> sources.map(newJLDExtraction(_).toJObject(publisher, true))) ~ // TODO: Instead, ask publisher for ref to mention
        ("destinations" -> targets.map(newJLDExtraction(_).toJObject(publisher, true)))
  }
}

class JLDUndirectedRelation(mention: Mention) extends JLDExtraction(mention) {
  
  def typename = "UndirectedRelation"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val arguments = mention.arguments.values.flatten // The keys are skipped
    val argumentMentions =
        if (arguments.isEmpty) None
        else Some(arguments.map(new JLDArgument(_).toJObject(publisher)).toList)
    val trigger = getTrigger()
    val triggerMention =
        if (!trigger.isDefined) None
        else Some(new JLDTrigger(trigger.get).toJObject(publisher))

    super.toJObject(publisher) ~
        (JLDTrigger.singular -> triggerMention) ~
        (JLDArgument.plural, argumentMentions)
  }
}

class JLDDependency(edge: (Int, Int, String), words: Seq[JLDWord]) extends JLDObject {

  def typename = "Dependency"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val source = words(edge._1).value
    val destination = words(edge._2).value
    
    publisher.mkType(this) ~
        publisher.mkRef("source", source) ~
        publisher.mkRef("destination", destination) ~
        ("relation" -> edge._3)
  }  
}

object JLDDependency {
  val singular = "dependency"
  val plural = "dependencies"
}

class JLDGraphMapPair(key: String, directedGraph: DirectedGraph[String], words: Seq[JLDWord]) extends JLDObject {
 
  def typename = "Dependencies"
  
  override def toJValue(publisher: JLDPublisher, asRef: Boolean = false): JValue = {
    val jldEdges = directedGraph.allEdges.map(new JLDDependency(_, words))
    
    new JArray(toJObjects(jldEdges, publisher))
  }
}

class JLDWord(sentence: Sentence, index: Int, text: Option[String]) extends JLDObject {
  
  def typename = "Word"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (!optionArray.isDefined) None
        else Option(optionArray.get(index))
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)
    val jldText =
        if (!text.isDefined) None
        else Option(text.get.substring(startOffset, endOffset))

    publisher.mkType(this) ~
        publisher.mkId(this) ~
        ("text" -> jldText) ~
        ("tag" -> getOrNone(sentence.tags)) ~
        ("entity" -> getOrNone(sentence.entities)) ~
        ("startOffset" -> startOffset) ~
        ("endOffset" -> endOffset) ~
        ("lemma" -> getOrNone(sentence.lemmas)) ~
        ("chunk" -> getOrNone(sentence.chunks))
  }
}

object JLDWord {
  val singular = "word"
  val plural = "words"
}

class JLDSentence(sentence: Sentence, text: Option[String]) extends JLDObject {
  
  def typename = "Sentence"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val key = "universal-enhanced"
    val jldWords = sentence.words.indices.map(new JLDWord(sentence, _, text))
    val dependencies = sentence.graphs.get(key)
    lazy val jldGraphMapPair = // Don't evaluate until words are published!
        if (!dependencies.isDefined) None
        else Some(new JLDGraphMapPair(key, dependencies.get, jldWords).toJValue(publisher))
          
    publisher.mkType(this) ~
        publisher.mkId(this) ~
        (JLDWord.plural -> toJObjects(jldWords, publisher)) ~
        ("text" -> sentence.getSentenceText()) ~
        (JLDDependency.plural -> jldGraphMapPair)
  }
}

object JLDSentence {
  val singular = "sentence"
  val plural = "sentences"
}

class JLDDocument(annotatedDocument: JLDObject.AnnotatedDocument) extends JLDObject(annotatedDocument) {
  
  def typename = "Document"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val jldSentences = annotatedDocument.document.sentences.map(new JLDSentence(_, annotatedDocument.document.text))
      
      publisher.mkType(this) ~
          publisher.mkId(this) ~
          (JLDSentence.plural -> toJObjects(jldSentences, publisher))
  }
}

object JLDDocument {
  val singular = "document"
  val plural = "documents"
}

class JLDCorpus(anthology: JLDObject.Anthology) extends JLDObject(anthology) {
  
  def typename = "Corpus"
  
  override def toJObject(publisher: JLDPublisher, asRef: Boolean = false): JObject = {
    val jldDocuments = anthology.map(new JLDDocument(_))
    val jldExtractions = anthology.flatMap(_.mentions).map(newJLDExtraction(_))

    publisher.mkType(this) ~
        (JLDDocument.plural -> toJObjects(jldDocuments, publisher)) ~
        (JLDExtraction.plural -> toJObjects(jldExtractions, publisher))
  }
}

object JLDCorpus {
  val singular = "corpus"
  val plural = "corpora"
}
