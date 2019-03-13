package org.clulab.wm.eidos.serialization.json

import java.util.{IdentityHashMap => JIdentityHashMap}
import java.util.{Set => JavaSet}
import java.time.LocalDateTime

import org.clulab.odin.{Attachment, Mention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.{AnnotatedDocument, DCT, EidosDocument, TimeInterval}
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.groundings.{AdjectiveGrounder, AdjectiveGrounding, OntologyGrounding}
import org.clulab.wm.eidos.mentions.{EidosCrossSentenceMention, EidosEventMention, EidosMention, EidosTextBoundMention}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable

// This is an object than when asked to convert itself a JSON object or value, converts
// itself in a way that conforms to the JSON-LD standard as well.
abstract class JLDObject(val serializer: JLDSerializer, val typename: String, val value: Any = new Object()) {
  serializer.register(this)
  
  def serialize(): JValue = serializer.serialize(this)

  def serialize(adjectiveGrounder: AdjectiveGrounder): JValue = {
    val oldAdjectiveGrounder = serializer.adjectiveGrounder

    serializer.adjectiveGrounder = Option(adjectiveGrounder)

    val result = serialize()

    serializer.adjectiveGrounder = oldAdjectiveGrounder
    result
  }
  
  def toJsonStr: String =
      pretty(render(serialize()))

  def toJObject: TidyJObject
  
  def newJLDExtraction(mention: EidosMention): JLDExtraction = mention match {
    case mention: EidosEventMention => JLDRelation.newJLDRelation(serializer, mention)
    //case mention: EidosRelationMention =>
    case mention: EidosCrossSentenceMention => JLDRelation.newJLDRelation(serializer, mention)
    case mention: EidosTextBoundMention => new JLDConceptEntity(serializer, mention)
    case _ => throw new IllegalArgumentException("Unknown Mention: " + mention)
  }

  def isExtractable(mention: EidosMention) = true

  def newJLDAttachment(attachment: Attachment): JLDAttachment =
      EidosAttachment.asEidosAttachment(attachment).newJLDAttachment(serializer)
}

// This class helps serialize/convert a JLDObject to JLD by keeping track of
// what types are included and providing IDs so that references to can be made
// within the JSON structure.
class JLDSerializer(var adjectiveGrounder: Option[AdjectiveGrounder]) {
  protected val typenames: mutable.HashSet[String] = mutable.HashSet[String]()
  protected val typenamesByIdentity: JIdentityHashMap[Any, String] = new JIdentityHashMap[Any, String]()
  protected val idsByTypenameByIdentity: mutable.HashMap[String, JIdentityHashMap[Any, Int]] = mutable.HashMap()
  protected val jldObjectsByTypenameByIdentity: mutable.HashMap[String, JIdentityHashMap[JLDObject, Int]] = mutable.HashMap()

  def register(jldObject: JLDObject): Unit = {
    val identity = jldObject.value
    val typename = jldObject.typename

    typenamesByIdentity.put(identity, typename) // So that know which idsByTypenamesByIdentity to look in

    val idsByIdentity = idsByTypenameByIdentity.getOrElseUpdate(typename, new JIdentityHashMap[Any, Int]())

    if (!idsByIdentity.containsKey(identity))
      idsByIdentity.put(identity, idsByIdentity.size() + 1)

    val jldObjectsByIdentity = jldObjectsByTypenameByIdentity.getOrElseUpdate(typename, new JIdentityHashMap[JLDObject,Int]())

    jldObjectsByIdentity.put(jldObject, 0)
  }

  def byTypename(typename: String): JavaSet[JLDObject] = jldObjectsByTypenameByIdentity(typename).keySet()

  protected def mkId(typename: String, id: Int): JField =
      new JField("@id", s"_:${typename}_$id")

  def mkId(jldObject: JLDObject): JField = {
    val identity = jldObject.value
    val typename = jldObject.typename

    typenamesByIdentity.put(identity, typename) // So that know which idsByTypenamesByIdentity to look in

    val idsByIdentity = idsByTypenameByIdentity.getOrElseUpdate(typename, new JIdentityHashMap[Any, Int]())
    val id = idsByIdentity.get(identity)

    mkId(typename, id)
  }

  protected def mkType(typename: String): JField = {
    typenames += typename
    "@type" -> typename
  }

  def mkType(jldObject: JLDObject): JField = mkType(jldObject.typename)

  def mkContext(): TidyJObject = {
    // The wiki turns <a id="Document"> into <a id="user-content-document">
    // but w3id.org is not set up to lowercase the document, so it is done here in code.
    def mkContext(name: String): JField = new JField(name, JLDSerializer.base + name.toLowerCase())

    val types: List[JField] = typenames.toList.sorted.map(mkContext)

    new TidyJObject(types)
  }

  def mkRef(identity: Any): TidyJObject = {
    val typename = Option(typenamesByIdentity.get(identity))
        .getOrElse(throw new Exception("Cannot make reference to unknown identity: " + identity))
    val id = idsByTypenameByIdentity(typename).get(identity)

    val field: JField = mkId(typename, id)

    new TidyJObject(List(field))
  }

  def serialize(jldObjectProvider: JLDObject): JValue = {
    // This must be done first in order to collect the context entries
    val jObject: TidyJObject = jldObjectProvider.toJObject

    TidyJObject(List(
      "@context" -> mkContext
    )) + jObject
  }
}

object JLDSerializer {
  val base = "https://w3id.org/wm/cag/"
}

class JLDArgument(serializer: JLDSerializer, typeString: String, mention: EidosMention)
    extends JLDObject(serializer, "Argument") {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "type" -> typeString,
    "value" -> serializer.mkRef(mention)
  ))
}

object JLDArgument {
  val singular = "argument"
  val plural = "arguments"
}

class JLDOntologyGrounding(serializer: JLDSerializer, name: String, value: Float)
    extends JLDObject(serializer, "Grounding") {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "ontologyConcept" -> name,
    "value" -> value
  ))
}

object JLDOntologyGrounding {
  val singular = "grounding"
  val plural: String = singular // Mass noun
}

class JLDOntologyGroundings(serializer: JLDSerializer, name: String, grounding: OntologyGrounding)
    extends JLDObject(serializer, "Groundings") {
  val jldGroundings: Seq[JObject] = grounding.grounding.map(pair => new JLDOntologyGrounding(serializer, pair._1.name, pair._2).toJObject)

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "name" -> name,
    "values" -> jldGroundings
  ))
}

object JLDOntologyGroundings {
  val singular = "groundings"
  val pural: String = singular
}

class JLDModifier(serializer: JLDSerializer, quantifier: String, mention: Option[Mention])
    extends JLDObject(serializer, JLDModifier.typename) {

  override def toJObject: TidyJObject = {
    val grounding = serializer.adjectiveGrounder.map(_.groundAdjective(quantifier)).getOrElse(AdjectiveGrounding.noAdjectiveGrounding)
    val jldProvenance = mention.map(mention => new JLDProvenance(serializer, mention).toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      "text" -> quantifier,
      JLDProvenance.singular -> jldProvenance,
      "intercept" -> grounding.intercept,
      "mu" -> grounding.mu,
      "sigma" -> grounding.sigma
    ))
  }
}

object JLDModifier {
  val singular = "modifier"
  val plural = "modifiers"
  val typename = "Modifier"
}

abstract class JLDAttachment(serializer: JLDSerializer, kind: String)
    extends JLDObject(serializer, "State") {
}

object JLDAttachment {
  val singular = "state"
  val plural = "states"
}

class JLDTriggeredAttachment(serializer: JLDSerializer, kind: String, triggeredAttachment: TriggeredAttachment)
    extends JLDAttachment(serializer, "State") {

  override def toJObject: TidyJObject = {
    val text = triggeredAttachment.trigger
    val jldProvanance = triggeredAttachment.getTriggerMention.map(mention => new JLDProvenance(serializer, mention).toJObject)
    val jldModifiers =
        if (triggeredAttachment.quantifiers.isEmpty) Seq.empty
        else
          triggeredAttachment.quantifiers.get.indices.map { index =>
            val quantifier = triggeredAttachment.quantifiers.get(index)
            val quantifierMention =
              if (triggeredAttachment.getQuantifierMentions.isDefined) Some(triggeredAttachment.getQuantifierMentions.get(index))
              else None

            new JLDModifier(serializer, quantifier, quantifierMention).toJObject
          }

    TidyJObject(List(
      serializer.mkType(this),
      "type" -> kind,
      "text" -> text,
      JLDProvenance.singular -> jldProvanance,
      JLDModifier.plural -> jldModifiers
    ))
  }
}

class JLDContextAttachment(serializer: JLDSerializer, kind: String, contextAttachment: ContextAttachment)
    extends JLDAttachment(serializer, "State") {

  override def toJObject: TidyJObject = {
    val value = serializer.mkRef(contextAttachment.value)
    val text = contextAttachment.text

    TidyJObject(List(
      serializer.mkType(this),
      "type" -> kind,
      "text" -> text,
      "value" -> value
    ))
  }
}

// TODO: This format is not documented, nor is it used AFAICT.
class JLDScoredAttachment(serializer: JLDSerializer, kind: String, scoredAttachment: Score)
  extends JLDAttachment(serializer, "Score") {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "type" -> kind,
    "value" -> scoredAttachment.score
  ))
}

class JLDInterval(serializer: JLDSerializer, interval: Interval, offset: Int, inclusiveEnd: Boolean)
    extends JLDObject(serializer, "Interval") {

  override def toJObject: TidyJObject = {
    val endCorrection = if (inclusiveEnd) -1 else 0

    TidyJObject(List(
      serializer.mkType(this),
      "start" -> (interval.start + offset),
      "end" -> (interval.end + offset + endCorrection)
    ))
  }
}

object JLDInterval {
//  There are many kinds of intervals, so this generic version is not being used.
//  val singular = "position"
//  val plural = "positions"
}

class JLDProvenance(serializer: JLDSerializer, mention: Mention)
    // Do not include the mention here because provenances are not to be referenced!
    extends JLDObject(serializer, "Provenance") {

  def this(serializer: JLDSerializer, eidosMention: EidosMention) = this(serializer, eidosMention.odinMention)

  override def toJObject: TidyJObject = {
    val document = mention.document
    val sentence = mention.sentenceObj
    val tokenInterval = mention.tokenInterval
    val documentCharInterval = {
      val start = sentence.startOffsets(tokenInterval.start)
      val end = sentence.endOffsets(tokenInterval.end - 1)

      Interval(start, end)
    }

    TidyJObject(List(
      serializer.mkType(this),
      JLDDocument.singular -> serializer.mkRef(document),
      "documentCharPositions" -> Seq(new JLDInterval(serializer, documentCharInterval, offset = 0, inclusiveEnd = true).toJObject),
      JLDSentence.singular -> serializer.mkRef(sentence),
      "sentenceWordPositions" -> Seq(new JLDInterval(serializer, tokenInterval, offset = 1, inclusiveEnd = true).toJObject)
    ))
  }
}

object JLDProvenance {
  val singular = "provenance"
  val plural = "provenances"
}

class JLDTrigger(serializer: JLDSerializer, mention: EidosMention)
    extends JLDObject(serializer, "Trigger", mention) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "text" -> mention.odinMention.text,
    JLDProvenance.singular -> Seq(new JLDProvenance(serializer, mention).toJObject)
  ))
}

object JLDTrigger {
  val singular = "trigger"
  val plural = "triggers"
}

abstract class JLDExtraction(serializer: JLDSerializer, typeString: String, val subtypeString: String, mention: EidosMention)
    extends JLDObject(serializer, JLDExtraction.typename, mention) {

  def getMentions: Seq[EidosMention] =  Seq.empty
  // This isn't necessary because attachments only show provenance, not reference to a different extraction
  //mention.eidosMentionsFromAttachments

  protected def provenance(): Seq[JValue] = Seq(new JLDProvenance(serializer, mention).toJObject)

  override def toJObject: TidyJObject = {
    val jldAttachments = mention.odinMention.attachments.toSeq
        .collect{ case a: TriggeredAttachment => a }
        .sortWith(TriggeredAttachment.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldTimeAttachments = mention.odinMention.attachments.toSeq
        .collect{ case a: Time => a }
        .sortWith(Time.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldLocationAttachments = mention.odinMention.attachments.toSeq
        .collect{ case a: Location => a }
        .sortWith(Location.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldDctAttachments = mention.odinMention.attachments.toSeq
        .collect{ case a: DCTime => a }
        .sortWith(DCTime.lessThan)
        .map(attachment => newJLDAttachment(attachment))

    // This might be used to test some groundings when they aren't configured to be produced.
    //val ontologyGroundings = mention.grounding.values.flatMap(_.grounding).toSeq
    //val ontologyGrounding = new OntologyGrounding(Seq(("hello", 4.5d), ("bye", 1.0d))).grounding
    val jldGroundings = mention.grounding.map(pair => new JLDOntologyGroundings(serializer, pair._1, pair._2).toJObject).toSeq
    val jldAllAttachments = (jldAttachments ++ jldTimeAttachments ++ jldLocationAttachments ++ jldDctAttachments).map(_.toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "type" -> typeString,
      "subtype" -> subtypeString,
      "labels" -> mention.odinMention.labels,
      "text" -> mention.odinMention.text,
      "rule" -> mention.odinMention.foundBy,
      "canonicalName" -> mention.canonicalName,
      "groundings" -> jldGroundings,
      JLDProvenance.singular -> provenance(),
      JLDAttachment.plural -> jldAllAttachments
    ))
  }
}

object JLDExtraction {
  val typename = "Extraction"
  val singular = "extraction"
  val plural = "extractions"
}

class JLDConcept(serializer: JLDSerializer, subtypeString: String, mention: EidosMention)
    extends JLDExtraction(serializer, JLDConcept.typeString, subtypeString, mention) {
}

object JLDConcept {
  val typeString = "concept"
}

class JLDConceptEntity(serializer: JLDSerializer, mention: EidosMention)
  extends JLDConcept(serializer, JLDConceptEntity.subtypeString, mention) {
}

object JLDConceptEntity {
  val subtypeString = "entity"
}

class JLDRelation(serializer: JLDSerializer, subtypeString: String, mention: EidosMention)
  extends JLDExtraction(serializer, JLDRelation.typeString, subtypeString, mention) {
}

object JLDRelation {
  val typeString = "relation"

  def newJLDRelation(serializer: JLDSerializer, mention: EidosEventMention): JLDRelation = {
    // This could be looked up in the taxonomy somehow, but the taxonomy doesn't include
    // information about the name of the arguments anyway, so may as well do all decoding here.
    // This could be pushed down to JLDRelation given EidosEventMention => JLDRelation.
    // If the JSON-LD specification doesn't change, then it is possible for the argument
    // names to be specified in master.yml file and then be taken over verbatim by querying the
    // arguments dictionary.
    if (JLDRelationCorrelation.taxonomy == mention.odinMention.label)
      new JLDRelationCorrelation(serializer, mention)
    else if (JLDRelationCausation.taxonomy == mention.odinMention.label)
      new JLDRelationCausation(serializer, mention)
    else
      throw new IllegalArgumentException("Unknown Mention: " + mention)
  }

  def newJLDRelation(serializer: JLDSerializer, mention: EidosCrossSentenceMention): JLDRelation = {
    // Cross sentence mentions are always a coreference.
    if (JLDRelationCoreference.taxonomy == mention.odinMention.label)
      new JLDRelationCoreference(serializer, mention)
    else
      throw new IllegalArgumentException("Unknown Mention: " + mention)
  }
}

class JLDRelationCausation(serializer: JLDSerializer, mention: EidosEventMention)
    extends JLDRelation(serializer, JLDRelationCausation.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationCausation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCausation.effect, Seq.empty).filter(isExtractable)

    sources ++ targets ++ super.getMentions
  }

  override def toJObject: TidyJObject = {
    val trigger = new JLDTrigger(serializer, mention.eidosTrigger).toJObject
    val sources = mention.eidosArguments.getOrElse(JLDRelationCausation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCausation.effect, Seq.empty).filter(isExtractable)
    val jldArguments =
        sources.map(new JLDArgument(serializer, "source", _).toJObject) ++
        targets.map(new JLDArgument(serializer, "destination", _).toJObject)

    super.toJObject + TidyJObject(List(
      JLDTrigger.singular -> trigger,
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationCausation {
  val subtypeString = "causation"
  val taxonomy = "Causal"
  val cause = "cause"
  val effect = "effect"
}

class JLDRelationCorrelation(serializer: JLDSerializer, mention: EidosEventMention)
  extends JLDRelation(serializer, JLDRelationCorrelation.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationCorrelation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCorrelation.effect, Seq.empty).filter(isExtractable)

    sources ++ targets ++ super.getMentions
  }

  override def toJObject: TidyJObject = {
    val trigger = new JLDTrigger(serializer, mention.eidosTrigger).toJObject
    val sources = mention.eidosArguments.getOrElse(JLDRelationCorrelation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCorrelation.effect, Seq.empty).filter(isExtractable)
    val jldArguments =
        sources.map(new JLDArgument(serializer, "argument", _).toJObject) ++
        targets.map(new JLDArgument(serializer, "argument", _).toJObject)

    super.toJObject + TidyJObject(List(
      JLDTrigger.singular -> trigger,
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationCorrelation {
  val subtypeString = "correlation"
  val taxonomy = "Correlation"
  val cause = "cause"
  val effect = "effect"
}

class JLDRelationCoreference(serializer: JLDSerializer, mention: EidosCrossSentenceMention)
  extends JLDRelation(serializer, JLDRelationCoreference.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] =
      Seq(mention.eidosAnchor, mention.eidosNeighbor) ++ super.getMentions

  // The provenance of this mention is just that of anchor and neighbor.
  override protected def provenance(): Seq[JValue] = Seq(
      new JLDProvenance(serializer, mention.eidosAnchor).toJObject,
      new JLDProvenance(serializer, mention.eidosNeighbor).toJObject
  )

  override def toJObject: TidyJObject = {
    val jldArguments = Seq(
        new JLDArgument(serializer, "anchor", mention.eidosAnchor).toJObject,
        new JLDArgument(serializer, "reference", mention.eidosNeighbor).toJObject
    )

    super.toJObject + TidyJObject(List(
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationCoreference {
  val subtypeString = "coreference"
  val taxonomy = "Coreference"
}

class JLDDependency(serializer: JLDSerializer, edge: (Int, Int, String), words: Seq[JLDWord])
    extends JLDObject(serializer, JLDDependency.typename) {

  override def toJObject: TidyJObject = {
    val source = words(edge._1).value
    val destination = words(edge._2).value
    val relation = edge._3

    TidyJObject(List(
      serializer.mkType(this),
      "source" -> serializer.mkRef(source),
      "destination" -> serializer.mkRef(destination),
      "relation" -> relation
    ))
  }
}

object JLDDependency {
  val singular = "dependency"
  val plural = "dependencies"
  val typename = "Dependency"
}

class JLDGraphMapPair(serializer: JLDSerializer, key: String, directedGraph: DirectedGraph[String], words: Seq[JLDWord])
    extends JLDObject(serializer, "Dependencies") {

  def toJObject: TidyJObject = TidyJObject()

  def toJValue: JValue = {
    val jldEdges = directedGraph.allEdges.map(new JLDDependency(serializer, _, words).toJObject)

    new JArray(jldEdges)
  }
}

class JLDWord(serializer: JLDSerializer, val document: Document, val sentence: Sentence, val index: Int)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDWord.typename) {

  override def toJObject: TidyJObject = {
    def getOrNone(optionArray: Option[Array[String]]): Option[String] = optionArray.map(values => values(index))

    val startOffset = sentence.startOffsets(index)
    val endOffset = sentence.endOffsets(index)
    val jldText: Option[String] = document.text.map(text => text.substring(startOffset, endOffset))

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "text" -> jldText,
      "tag" -> getOrNone(sentence.tags),
      "entity" -> getOrNone(sentence.entities),
      "startOffset" -> startOffset,
      "endOffset" -> endOffset,
      "lemma" -> getOrNone(sentence.lemmas),
      "chunk" -> getOrNone(sentence.chunks),
      "norm" -> getOrNone(sentence.norms)
    ))
  }
}

object JLDWord {
  val singular = "word"
  val plural = "words"
  val typename = "Word"
}

class JLDTimeInterval(serializer:JLDSerializer, val start: LocalDateTime, val end: LocalDateTime, val duration: Long)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDTimeInterval.typename) {
  
  override def toJObject: TidyJObject = {
    val startDateTime = Option(start).map(_.toString)
    val endDateTime = Option(end).map(_.toString)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "start" -> startDateTime,
      "end" -> endDateTime,
      "duration" -> duration
    ))
  }
}

object JLDTimeInterval {
  val singular = "interval"
  val plural = "intervals"
  val typename = "TimeInterval"
}


class JLDTimex(serializer: JLDSerializer, val interval: TimeInterval)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDTimex.typename, interval) {
  
  override def toJObject: TidyJObject = {
    val jldIntervals = interval.intervals.map(timeStep => new JLDTimeInterval(serializer, timeStep.start, timeStep.end, timeStep.duration).toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "startOffset" -> interval.span.start,
      "endOffset" -> interval.span.end,
      "text" -> interval.text,
      JLDTimeInterval.plural -> jldIntervals
    ))
  }
}

object JLDTimex {
  val singular = "timex"
  val plural = "timexes"
  val typename = "TimeExpression"
}

class JLDGeoID(serializer:JLDSerializer, val geoid: GeoPhraseID)
// The document, sentence, index above will be used to recognized words.
  extends JLDObject(serializer, JLDGeoID.typename, geoid) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    serializer.mkId(this),
    "startOffset" -> geoid.startOffset,
    "endOffset" -> geoid.endOffset,
    "text" -> geoid.text,
    "geoID" -> geoid.geonameID.map(_.toString)
    // JLDTimeInterval.plural -> toJObjects(jldIntervals)
  ))
}

object JLDGeoID {
  val singular = "geoloc"
  val plural = "geolocs"
  val typename = "GeoLocation"
}

class JLDDCT(serializer: JLDSerializer, val dct: DCT)
// The document, sentence, index above will be used to recognized words.
  extends JLDObject(serializer, JLDDCT.typename, dct) {

  override def toJObject: TidyJObject = {
    val text = Option(dct.text)
    val start = if (dct.interval.isDefined) Some(dct.interval.start.toString) else None
    val end = if (dct.interval.isDefined) Some(dct.interval.end.toString) else None

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "text" -> text,
      "start" -> start,
      "end" -> end
    ))
  }
}

object JLDDCT {
  val singular = "dct"
  val typename = "DCT"
}

class JLDSentence(serializer: JLDSerializer, document: Document, sentence: Sentence)
    extends JLDObject(serializer, JLDSentence.typename, sentence) {

  override def toJObject: TidyJObject = {
    val key = GraphMap.UNIVERSAL_ENHANCED
    val jldWords = sentence.words.indices.map(new JLDWord(serializer, document, sentence, _))
    val dependencies = sentence.graphs.get(key)
    val sent_id = document.sentences.indexOf(sentence)
    val timexes: Option[Seq[JObject]] = document.asInstanceOf[EidosDocument].times.map {
      times => times(sent_id).map { time => new JLDTimex(serializer, time).toJObject }
    }
    val geoExps: Option[Seq[JObject]] = document.asInstanceOf[EidosDocument].geolocs.map {
      geolocs => geolocs(sent_id).map { geoloc => new JLDGeoID(serializer, geoloc).toJObject }
    }
    // This is given access to the words because they are nicely in order and no searching need be done.
    val jldGraphMapPair = dependencies.map(dependency => new JLDGraphMapPair(serializer, key, dependency, jldWords).toJValue)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "text" -> sentence.getSentenceText,
      JLDWord.plural -> jldWords.map(_.toJObject),
      JLDDependency.plural -> jldGraphMapPair,
      JLDTimex.plural -> timexes,
      JLDGeoID.plural -> geoExps
    ))
  }
}

object JLDSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JLDDocument(serializer: JLDSerializer, annotatedDocument: AnnotatedDocument)
    extends JLDObject(serializer, JLDDocument.typename, annotatedDocument.document) {

  override def toJObject: TidyJObject = {
    val jldSentences = annotatedDocument.document.sentences.map(new JLDSentence(serializer, annotatedDocument.document, _).toJObject).toSeq
    val jldText = annotatedDocument.document.text.map(text => text)
    val dct = annotatedDocument.document.asInstanceOf[EidosDocument].dct
    val jldDCT = dct.map(new JLDDCT(serializer, _).toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "title" -> annotatedDocument.document.id,
      "text" -> jldText,
      "dct" -> jldDCT,
      JLDSentence.plural -> jldSentences
    ))
  }
}

object JLDDocument {
  val singular = "document"
  val plural = "documents"
  val typename = "Document"
}

class JLDCorpus protected (serializer: JLDSerializer, corpus: Corpus) extends JLDObject(serializer, JLDCorpus.typename, corpus) {

  protected def this(corpus: Corpus, adjectiveGrounder: Option[AdjectiveGrounder]) = this(new JLDSerializer(adjectiveGrounder), corpus)

  // Traditional, expert call that some may still be using that includes an adjective grounder from Eidos or now from elsewhere
  def this(corpus: Corpus, adjectiveGrounder: AdjectiveGrounder) = this(corpus, Option(adjectiveGrounder))

  // New call used in examples so that AdjectiveGrounder can be ignored
  def this(corpus: Corpus) = this(corpus, Option.empty[AdjectiveGrounder])

  def this(annotatedDocument: AnnotatedDocument) = this(Seq(annotatedDocument))

  protected def collectMentions(mentions: Seq[EidosMention], mapOfMentions: JIdentityHashMap[EidosMention, Int]): Seq[JLDExtraction] = {
    val newMentions = mentions.filter(isExtractable).filter { mention =>
      if (mapOfMentions.containsKey(mention))
        false
      else {
        mapOfMentions.put(mention, mapOfMentions.size() + 1)
        true
      }
    }

    newMentions.flatMap { mention =>
      // Add these in parent, children, parent, children order instead of
      // the previously used parents, children, children order.
      val jldExtraction = newJLDExtraction(mention)
      val recMentions = jldExtraction.getMentions
      val jldExtractions = jldExtraction +: collectMentions(recMentions, mapOfMentions)

      jldExtractions
    }
  }

  protected def collectMentions(mentions: Seq[EidosMention]): Seq[JLDExtraction] = {
    val ordering = Array(
        JLDConceptEntity.subtypeString,
        JLDRelationCausation.subtypeString,
        JLDRelationCorrelation.subtypeString,
        JLDRelationCoreference.subtypeString
    )

    val mapOfMentions = new JIdentityHashMap[EidosMention, Int]()

    def lt(left: JLDExtraction, right: JLDExtraction): Boolean = {
      val leftOrdering = ordering.indexOf(left.subtypeString)
      val rightOrdering = ordering.indexOf(right.subtypeString)
      
      if (leftOrdering != rightOrdering)
        leftOrdering < rightOrdering
      else
        mapOfMentions.get(left.value) < mapOfMentions.get(right.value)
    }

    collectMentions(mentions, mapOfMentions).sortWith(lt)
  }
  
  override def toJObject: TidyJObject = {
    val jldDocuments = corpus.map(new JLDDocument(serializer, _).toJObject)
    val eidosMentions = corpus.flatMap(_.eidosMentions).sortWith(EidosMention.before) // At least start out in order
    val jldExtractions = collectMentions(eidosMentions).map(_.toJObject)

//    val index1 = 0.until(mentions.size).find(i => mentions(i).matches("DirectedRelation"))
//    if (index1.isDefined) {
//      val position1 = mentions(index1.get).end
//      println("position1 " + position1)
//     
//      val index2 = index1.get + 1
//      if (index2 < mentions.size) {
//        val position2 = mentions(index2).end
//        println("position2 " + position2)
//      }
//    }

    TidyJObject(List(
      serializer.mkType(this),
      JLDDocument.plural -> jldDocuments,
      JLDExtraction.plural -> jldExtractions
    ))
  }
}

object JLDCorpus {
  val singular = "corpus"
  val plural = "corpora"
  val typename = "Corpus"
}
