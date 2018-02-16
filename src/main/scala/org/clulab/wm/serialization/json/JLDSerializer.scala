package org.clulab.wm.serialization.json

import java.util.IdentityHashMap // Unfortunately borrowed from Java
import java.util.{Set => JavaSet} // Holds keys of IdentityHashMap

import scala.collection.JavaConverters._
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
abstract class JLDObject(val serializer: JLDSerializer, val typename: String, val value: Any = new Object()) {
  serializer.register(this)
  
  def serialize() = serializer.serialize(this)
  
  // This is for individual objects
  def toJObject(): JObject = ???
  
  // This is for arrays of values
  def toJValue(): JValue = ???
  
  def noneIfEmpty(values: Seq[JValue]) =
      if (values.isEmpty) None
      else Some(values)
  
  def toJObjects(jldObjects: Seq[JLDObject]): List[JObject] =
      jldObjects.map(_.toJObject()).toList
  
  // TODO: This should use the yaml file to distinguish
  def newJLDExtraction(mention: Mention): JLDExtraction = {
    val causes = mention.arguments.getOrElse(JLDObject.cause, Nil)
    val effects = mention.arguments.getOrElse(JLDObject.effect, Nil)
    val others = mention.arguments.keySet.exists(key => key != JLDObject.cause && key != JLDObject.effect)
    
    if (!causes.isEmpty && !effects.isEmpty)
      new JLDDirectedRelation(serializer, mention)
    else if (others || !causes.isEmpty || !effects.isEmpty)
      new JLDUndirectedRelation(serializer, mention)
    else
      new JLDEntity(serializer, mention)
  }
  
  def newJLDAttachment(attachment: Attachment, mention: Mention): JLDAttachment = attachment match {
    case attachment: Quantification => new JLDAttachment(serializer, "QUANT", attachment.quantifier, attachment.adverbs, mention: Mention)
    case attachment: Increase => new JLDAttachment(serializer, "INC", attachment.trigger, attachment.quantifier, mention: Mention)
    case attachment: Decrease => new JLDAttachment(serializer, "DEC", attachment.trigger, attachment.quantifier, mention: Mention)
  }
}

object JLDObject {
  case class AnnotatedDocument(var document: Document, var mentions: Seq[Mention])
  type Corpus = Seq[AnnotatedDocument]
  
  val cause = "cause"
  val effect = "effect"
}

// This class helps serialize/convert a JLDObject to JLD by keeping track of
// what types are included and providing IDs so that references to can be made
// within the JSON structure.
class JLDSerializer() {
  protected val typenames = mutable.HashSet[String]()
  protected val typenamesByIdentity = new IdentityHashMap[Any, String]()
  protected val idsByTypenameByIdentity: mutable.HashMap[String, IdentityHashMap[Any, Int]] = mutable.HashMap()
  protected val jldObjectsByTypenameByIdentity: mutable.HashMap[String, IdentityHashMap[JLDObject, Int]] = mutable.HashMap()
  
  def register(jldObject: JLDObject) = {
    val identity = jldObject.value
    val typename = jldObject.typename
    
    typenamesByIdentity.put(identity, typename) // So that know which idsByTypenamesByIdentity to look in
    
    val idsByIdentity = idsByTypenameByIdentity.getOrElseUpdate(typename, new IdentityHashMap[Any, Int]())

    if (!idsByIdentity.containsKey(identity))
      idsByIdentity.put(identity, idsByIdentity.size() + 1)

    val jldObjectsByIdentity = jldObjectsByTypenameByIdentity.getOrElseUpdate(typename, new IdentityHashMap[JLDObject,Int]())
    
    jldObjectsByIdentity.put(jldObject, 0)    
  }

  def byTypename(typename: String): JavaSet[JLDObject] = jldObjectsByTypenameByIdentity(typename).keySet()
      
  protected def mkId(typename: String, id: Int): (String, String) =
      ("@id" -> ("_:" + typename + "_" + id))
    
  def mkId(jldObject: JLDObject): (String, String) = {
    val identity = jldObject.value
    val typename = jldObject.typename
    
    typenamesByIdentity.put(identity, typename) // So that know which idsByTypenamesByIdentity to look in
    
    val idsByIdentity = idsByTypenameByIdentity.getOrElseUpdate(typename, new IdentityHashMap[Any, Int]())
    val id = idsByIdentity.get(identity)
    
    mkId(typename, id)
  }
  
  protected def mkType(typename: String): (String, String) = {
    typenames += typename
    ("@type" -> typename)
  }
  
  def mkType(jldObject: JLDObject): (String, String) = mkType(jldObject.typename)

  def mkContext(): JObject = {
    def mkContext(name: String): JField = new JField(name, "#user-content-" + name.toLowerCase())
    
    val base = new JField("@base", JLDSerializer.base)
    val types = typenames.toList.sorted.map(mkContext(_))
    
    new JObject(base +: types)
  }
  
  def mkRef(identity: Any): JObject = {
    val typename = typenamesByIdentity.get(identity)
    if (typename == null)
      return mkId("UnknownType", 0)
      //throw new Exception("Cannot make reference to " + identity)
    
    val id = idsByTypenameByIdentity(typename).get(identity)
    
    mkId(typename, id)
  }
    
  def serialize(jldObjectProvider: JLDObject): JValue = {
    // This must be done first in order to collect the context entries
    val jObject = jldObjectProvider.toJObject()
    
    ("@context" -> mkContext) ~
        jObject
  }
}

object JLDSerializer {
  val base = "https://github.com/clulab/eidos/wiki/JSON-LD"
}

class JLDArgument(serializer: JLDSerializer, mention: Mention)
    extends JLDObject(serializer, "Argument", mention) {
  
  override def toJObject(): JObject =
    serializer.mkRef(mention)
}

object JLDArgument {
  val singular = "argument"
  val plural = "arguments"
}

class JLDModifier(serializer: JLDSerializer, text: String, mention: Mention)
    extends JLDObject(serializer, "Modifier") {

  override def toJObject(): JObject = {
      serializer.mkType(this) ~
          ("text" -> text) ~
          // This is not the mention you are looking for
          //(JLDProvenance.singular -> new JLDProvenance(serializer, mention).toJObject()) ~
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

class JLDAttachment(serializer: JLDSerializer, kind: String, text: String, modifiers: Option[Seq[Quantifier]], mention: Mention)
    extends JLDObject(serializer, "State") {
  
  override def toJObject(): JObject = {
    val jldModifiers =
        if (!modifiers.isDefined) Nil
        else modifiers.get.map(new JLDModifier(serializer, _, mention).toJObject()).toList
    
    serializer.mkType(this) ~
        serializer.mkId(this) ~
        ("type", kind) ~
        ("text", text) ~
        // This is also not the mention you are looking for
        //(JLDProvenance.singular -> new JLDProvenance(serializer, mention).toJObject()) ~
        (JLDModifier.plural -> noneIfEmpty(jldModifiers))
  }
}

object JLDAttachment {
  val singular = "state"
  val plural = "states"
}

class JLDInterval(serializer: JLDSerializer, interval: Interval)
    extends JLDObject(serializer, "Position") {

  override def toJObject(): JObject =
      serializer.mkType(this) ~
          ("start", interval.start) ~
          ("end", interval.end)
}

object JLDInterval {
  val singular = "position"
  val plural = "positions"
}

class JLDProvenance(serializer: JLDSerializer, mention: Mention, skipPositions: Boolean = false)
    extends JLDObject(serializer, "Provenance", mention) {
  
  override def toJObject(): JObject = {
    val document = mention.document
    val sentence = mention.sentenceObj

    if (skipPositions) // For the states when we don't have them
      serializer.mkType(this) ~
        (JLDDocument.singular -> serializer.mkRef(document)) ~
        (JLDSentence.singular -> serializer.mkRef(sentence))
    else {
      // Try to find the matching ones by document, sentence, and position with eq
      val allJldWords = serializer.byTypename(JLDWord.typename).asScala.map(_.asInstanceOf[JLDWord])
      val filteredJldWords = mention.start.until(mention.end).map { i => // This is done to keep the words in order
        allJldWords.find(jldWord => jldWord.document.eq(document) && jldWord.sentence.eq(sentence) && i == jldWord.index)
      }.filter(_.isDefined).map(_.get)      
      val refJldWords = filteredJldWords.map(jldWord => serializer.mkRef(jldWord.value))
            
      serializer.mkType(this) ~
          ("references" -> refJldWords)
    }
  }
}

object JLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
}

class JLDTrigger(serializer: JLDSerializer, mention: Mention)
    extends JLDObject(serializer, "Trigger", mention) {
  
  override def toJObject(): JObject =
      ("text" -> mention.text) ~
          (JLDProvenance.singular -> new JLDProvenance(serializer, mention).toJObject())
}

object JLDTrigger {
  val singular = "trigger"
  val plural = "triggers"
}

abstract class JLDExtraction(serializer: JLDSerializer, typename: String, mention: Mention) extends JLDObject(serializer, typename, mention) {
 
  def getMentions(): Seq[Mention] = Nil
  
  override def toJObject(): JObject = {
    val jldAttachments = mention.attachments.map(newJLDAttachment(_, mention)).toList
          
    serializer.mkType(this) ~
        serializer.mkId(this) ~
        ("labels" -> mention.labels) ~
        ("text" -> mention.text) ~
        ("rule" -> mention.foundBy) ~
        ("score" -> None) ~ // Figure out how to look up?, maybe like the sigma
        (JLDProvenance.singular -> new JLDProvenance(serializer, mention).toJObject()) ~
        (JLDAttachment.plural -> noneIfEmpty(toJObjects(jldAttachments)))
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

class JLDEntity(serializer: JLDSerializer, mention: Mention) 
    extends JLDExtraction(serializer, "Entity", mention) {
  
  override def toJObject(): JObject =
     super.toJObject()
}

class JLDDirectedRelation(serializer: JLDSerializer, mention: Mention)
    extends JLDExtraction(serializer, "DirectedRelation", mention) {

  override def getMentions(): Seq[Mention] = {
    val trigger = getTrigger()
    val triggers =
        if (!trigger.isDefined) Nil
        else Seq(trigger.get)
    val sources = mention.arguments.getOrElse(JLDObject.cause, Nil)
    val targets = mention.arguments.getOrElse(JLDObject.effect, Nil)
    
    triggers ++ sources ++ targets
  }
  
  override def toJObject(): JObject = {
    val trigger = getTrigger()
    val triggerMention =
        if (!trigger.isDefined) None
        else Some(serializer.mkRef(trigger.get))
    val sources = mention.arguments.getOrElse(JLDObject.cause, Nil)
    val targets = mention.arguments.getOrElse(JLDObject.effect, Nil)
 
    super.toJObject() ~
        (JLDTrigger.singular -> triggerMention) ~
        ("sources" -> sources.map(serializer.mkRef(_))) ~
        ("destinations" -> targets.map(serializer.mkRef(_)))
  }
}

class JLDUndirectedRelation(serializer: JLDSerializer, mention: Mention)
    extends JLDExtraction(serializer, "UndirectedRelation", mention) {
  
  override def getMentions(): Seq[Mention] = {
    val trigger = getTrigger()
    val triggers =
        if (!trigger.isDefined) Nil
        else Seq(trigger.get)
    val arguments = mention.arguments.values.flatten
    
    triggers ++ arguments
  }

  override def toJObject(): JObject = {
    val trigger = getTrigger()
    val triggerMention =
        if (!trigger.isDefined) None
        else Some(serializer.mkRef(trigger.get))
    val arguments = mention.arguments.values.flatten // The keys are skipped
    val argumentMentions =
        if (arguments.isEmpty) Nil
        else arguments.map(serializer.mkRef(_)).toList

    super.toJObject() ~
        (JLDTrigger.singular -> triggerMention) ~
        (JLDArgument.plural -> noneIfEmpty(argumentMentions))
  }
}

class JLDDependency(serializer: JLDSerializer, edge: (Int, Int, String), words: Seq[JLDWord])
    extends JLDObject(serializer, "Dependency") {

  override def toJObject(): JObject = {
    val source = words(edge._1).value
    val destination = words(edge._2).value
    
    serializer.mkType(this) ~
        ("source" -> serializer.mkRef(source)) ~
        ("destination" -> serializer.mkRef(destination)) ~
        ("relation" -> edge._3)
  }  
}

object JLDDependency {
  val singular = "dependency"
  val plural = "dependencies"
}

class JLDGraphMapPair(serializer: JLDSerializer, key: String, directedGraph: DirectedGraph[String], words: Seq[JLDWord])
    extends JLDObject(serializer, "Dependencies") {
 
  override def toJValue(): JValue = {
    val jldEdges = directedGraph.allEdges.map(new JLDDependency(serializer, _, words))
    
    new JArray(toJObjects(jldEdges))
  }
}

class JLDWord(serializer: JLDSerializer, val document: Document, val sentence: Sentence, val index: Int)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDWord.typename) {
  
  override def toJObject(): JObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] =
        if (!optionArray.isDefined) None
        else Option(optionArray.get(index))
     
    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)
    val jldText =
        if (!document.text.isDefined) None
        else Option(document.text.get.substring(startOffset, endOffset))

    serializer.mkType(this) ~
        serializer.mkId(this) ~
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
  val typename = "Word"
}

class JLDSentence(serializer: JLDSerializer, document: Document, sentence: Sentence)
    extends JLDObject(serializer, "Sentence") {
  
  override def toJObject(): JObject = {
    val key = "universal-enhanced"
    val jldWords = sentence.words.indices.map(new JLDWord(serializer, document, sentence, _))
    val dependencies = sentence.graphs.get(key)
    // This is given access to the words because they are nicely in order and no searching need be done.
    val jldGraphMapPair =
        if (!dependencies.isDefined) None
        else Some(new JLDGraphMapPair(serializer, key, dependencies.get, jldWords).toJValue())
          
    serializer.mkType(this) ~
        serializer.mkId(this) ~
        (JLDWord.plural -> noneIfEmpty(toJObjects(jldWords))) ~
        ("text" -> sentence.getSentenceText()) ~
        (JLDDependency.plural -> jldGraphMapPair)
  }
}

object JLDSentence {
  val singular = "sentence"
  val plural = "sentences"
}

class JLDDocument(serializer: JLDSerializer, annotatedDocument: JLDObject.AnnotatedDocument)
    extends JLDObject(serializer, "Document", annotatedDocument) {
  
  override def toJObject(): JObject = {
    val jldSentences = annotatedDocument.document.sentences.map(new JLDSentence(serializer, annotatedDocument.document, _))
      
      serializer.mkType(this) ~
          serializer.mkId(this) ~
          ("title" -> annotatedDocument.document.id) ~
          (JLDSentence.plural -> noneIfEmpty(toJObjects(jldSentences)))
  }
}

object JLDDocument {
  val singular = "document"
  val plural = "documents"
}

class JLDCorpus(serializer: JLDSerializer, anthology: JLDObject.Corpus)
    extends JLDObject(serializer, "Corpus", anthology) {
  
  def this(anthology: JLDObject.Corpus) = this(new JLDSerializer(), anthology)
  
  
  protected def collectMentions(mentions: Seq[Mention], mapOfMentions: IdentityHashMap[Mention, Int]): Seq[JLDExtraction] = {
    val newMentions = mentions.filter { mention => 
      if (mapOfMentions.containsKey(mention))
        false
      else {
        mapOfMentions.put(mention, 1)
        true
      }
    }
    
    if (!newMentions.isEmpty) {
      val jldExtractions = newMentions.map(newJLDExtraction(_))
      val recMentions = jldExtractions.flatMap(_.getMentions())
      
      newMentions.foreach(mapOfMentions.put(_, 1))
      collectMentions(recMentions, mapOfMentions)
      jldExtractions
    }
    else
      Nil
  }
  
  protected def collectMentions(mentions: Seq[Mention]): Seq[JLDExtraction] =
    collectMentions(mentions, new IdentityHashMap[Mention, Int]())
  
  override def toJObject(): JObject = {
    val jldDocuments = anthology.map(new JLDDocument(serializer, _))
    val mentions = anthology.flatMap(_.mentions)
    val jldExtractions = collectMentions(mentions)
    
    if (mentions.size != jldExtractions.size)
      println("There were hidden ones!")

    serializer.mkType(this) ~
        (JLDDocument.plural -> noneIfEmpty(toJObjects(jldDocuments))) ~
        (JLDExtraction.plural -> noneIfEmpty(toJObjects(jldExtractions)))
  }
}

object JLDCorpus {
  val singular = "corpus"
  val plural = "corpora"
}
