package org.clulab.wm.eidos.serialization.json

import java.util.{IdentityHashMap => JIdentityHashMap}
import java.util.{Set => JavaSet}
import java.time.LocalDateTime

import org.clulab.odin.EventMention
import org.clulab.odin.{Attachment, Mention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.document._
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.clulab.wm.eidos.groundings.{AdjectiveGrounder, AdjectiveGrounding, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosCrossSentenceEventMention
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
  
  def newJLDExtraction(mention: EidosMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): JLDExtraction = mention match {
//    case mention: EidosCrossSentenceEventMention => JLDRelation.newJLDRelation(serializer, mention, countAttachmentMap)
    case mention: EidosEventMention => JLDRelation.newJLDRelation(serializer, mention, countAttachmentMap)
    //case mention: EidosRelationMention =>
    case mention: EidosCrossSentenceMention => JLDRelation.newJLDRelation(serializer, mention, countAttachmentMap)
    case mention: EidosTextBoundMention => new JLDConceptEntity(serializer, mention, countAttachmentMap)
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

  def reorder(jldExtractions: Seq[JLDExtraction]): Unit = {
    if (jldExtractions.nonEmpty) {
      val idsByIdentity = idsByTypenameByIdentity(jldExtractions.head.typename)

      jldExtractions.zipWithIndex.foreach { case (jldExtraction, index) =>
        idsByIdentity.put(jldExtraction.eidosMention, index + 1)
      }
    }
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
    extends JLDObject(serializer, JLDArgument.typename) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "type" -> typeString,
    "value" -> serializer.mkRef(mention)
  ))
}

object JLDArgument {
  val singular = "argument"
  val plural = "arguments"
  val typename = "Argument"
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
  val jldGroundings: Seq[JObject] = grounding.grounding.map { case (namer, value) =>
    new JLDOntologyGrounding(serializer, namer.name, value).toJObject
  }
//  val versionOpt = if (name == "wm") Some("a1a6dbee0296bdd2b81a4a751fce17c9ed0a3af8") else None

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "name" -> name,
    "version" -> grounding.version,
    "versionDate" -> grounding.date.map(_.toString),
    "values" -> jldGroundings
  ))
}

object JLDOntologyGroundings {
  val singular = "groundings"
  val plural: String = singular
}

class JLDModifier(serializer: JLDSerializer, quantifier: String, provenance: Option[Provenance])
    extends JLDObject(serializer, JLDModifier.typename) {

  override def toJObject: TidyJObject = {
    val grounding = serializer.adjectiveGrounder.map(_.groundAdjective(quantifier)).getOrElse(AdjectiveGrounding.noAdjectiveGrounding)
    val jldProvenance = provenance.map(provenance => Seq(new JLDProvenance(serializer, provenance).toJObject))

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
    extends JLDObject(serializer, JLDAttachment.kind) {
}

object JLDAttachment {
  val singular = "state"
  val plural = "states"
  val kind = "State"
}

class JLDCountAttachment(serializer: JLDSerializer, val countAttachment: CountAttachment)
    extends JLDObject(serializer, JLDCountAttachment.typename, countAttachment) {

  override def toJObject: TidyJObject = {
    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "startOffset" -> countAttachment.startOffset,
      "endOffset" -> countAttachment.endOffset,
      "text" -> countAttachment.text,
      "value" -> countAttachment.migrationGroupCount.value,
      "modifier" -> countAttachment.migrationGroupCount.modifier.toString,
      "unit" -> countAttachment.migrationGroupCount.unit.toString
    ))
  }
}

object JLDCountAttachment {
  val singular = "count"
  val plural = "counts"
  val typename = "Count"
}

class JLDTriggeredAttachment(serializer: JLDSerializer, kind: String, triggeredAttachment: TriggeredAttachment)
    extends JLDAttachment(serializer, JLDAttachment.kind) {

  override def toJObject: TidyJObject = {
    val text = triggeredAttachment.trigger
    val jldProvanance = triggeredAttachment.triggerProvenance.map(provenance => Seq(new JLDProvenance(serializer, provenance).toJObject))
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
    extends JLDAttachment(serializer, JLDAttachment.kind) {

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
    extends JLDObject(serializer, JLDInterval.typename) {

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
  val typename = "Interval"
}

class JLDProvenance(serializer: JLDSerializer, provenance: Provenance)
    // Do not include the mention here because provenances are not to be referenced!
    extends JLDObject(serializer, JLDProvenance.typename) {

  def this(serializer: JLDSerializer, eidosMention: EidosMention) = this(serializer, Provenance(eidosMention.odinMention))

  override def toJObject: TidyJObject = {
    val document = provenance.document
    val sentence = document.sentences(provenance.sentence)
    val tokenInterval = provenance.interval
    val documentCharInterval = {
      val start = sentence.startOffsets(tokenInterval.start)
      // TODO: Especially in a CrossSentenceEventMention, the endOffset can be in a different sentence.
      // See CrossSentenceEventMention.text for how to get it.  It is complicated and too big a task for Provenance.
      val end = sentence.endOffsets(math.min(tokenInterval.end, sentence.endOffsets.length) - 1)

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
  val typename = "Provenance"
}

class JLDTrigger(serializer: JLDSerializer, mention: EidosMention)
    extends JLDObject(serializer, JLDTrigger.typename, mention) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "text" -> mention.odinMention.text,
    JLDProvenance.singular -> Seq(new JLDProvenance(serializer, mention).toJObject)
  ))
}

object JLDTrigger {
  val singular = "trigger"
  val plural = "triggers"
  val typename = "Trigger"
}

abstract class JLDExtraction(serializer: JLDSerializer, typeString: String, val subtypeString: String, val eidosMention: EidosMention,
    countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
    extends JLDObject(serializer, JLDExtraction.typename, eidosMention) {

  def getMentions: Seq[EidosMention] =  Seq.empty
  // This isn't necessary because attachments only show provenance, not reference to a different extraction
  //mention.eidosMentionsFromAttachments

  protected def provenance(): Seq[JValue] = Seq(new JLDProvenance(serializer, eidosMention).toJObject)

  override def toJObject: TidyJObject = {
    // Important: Since the attachments come from the odinMention and those mentions have not been
    // deduplicated like the eidosMentions have been, the attachments themselves can be duplicates.
    // However, in serialization of the attachments, like everything else, identity is used to make
    // the references.  This means that a duplicate, equals but not eq, count attribute will not be
    // found so that a reference cannot be generated and an exception will be thrown.  Perhaps
    // attachments need to be managed differently.
    val jldAttachments = eidosMention.odinMention.attachments.toSeq
        .collect{ case a: TriggeredAttachment => a }
        .sortWith(TriggeredAttachment.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldTimeAttachments = eidosMention.odinMention.attachments.toSeq
        .collect{ case a: Time => a }
        .sortWith(Time.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldLocationAttachments = eidosMention.odinMention.attachments.toSeq
        .collect{ case a: Location => a }
        .sortWith(Location.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldDctAttachments = eidosMention.odinMention.attachments.toSeq
        .collect{ case a: DCTime => a }
        .sortWith(DCTime.lessThan)
        .map(attachment => newJLDAttachment(attachment))
    val jldCountAttachments = eidosMention.odinMention.attachments.toSeq
        .collect{ case a: CountAttachment => a }
        .sortBy(countAttachment => countAttachment.startOffset)
        // Need to get the right JLD attachment from map
        .map(attachment => newJLDAttachment(countAttachmentMap(attachment).countAttachment)) // newJLDAttachment(attachment))

    // This might be used to test some groundings when they aren't configured to be produced.
    //val ontologyGroundings = mention.grounding.values.flatMap(_.grounding).toSeq
    //val ontologyGrounding = new OntologyGrounding(Seq(("hello", 4.5d), ("bye", 1.0d))).grounding

    val names = eidosMention.grounding.keys.toSeq.sorted
    val jldGroundings = names.map { name =>
      val grounding = eidosMention.grounding(name)

      new JLDOntologyGroundings(serializer, name, grounding).toJObject
    }
    val jldAllAttachments = (jldAttachments ++ jldTimeAttachments ++ jldLocationAttachments ++ jldDctAttachments ++ jldCountAttachments).map(_.toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "type" -> typeString,
      "subtype" -> subtypeString,
      "labels" -> eidosMention.odinMention.labels,
      "text" -> eidosMention.odinMention.text,
      "rule" -> eidosMention.odinMention.foundBy,
      "canonicalName" -> eidosMention.canonicalName,
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

class JLDConcept(serializer: JLDSerializer, subtypeString: String, mention: EidosMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
    extends JLDExtraction(serializer, JLDConcept.typeString, subtypeString, mention, countAttachmentMap) {
}

object JLDConcept {
  val typeString = "concept"
}

class JLDConceptEntity(serializer: JLDSerializer, mention: EidosMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
  extends JLDConcept(serializer, JLDConceptEntity.subtypeString, mention, countAttachmentMap) {
}

object JLDConceptEntity {
  val subtypeString = "entity"
}

class JLDRelation(serializer: JLDSerializer, subtypeString: String, mention: EidosMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
  extends JLDExtraction(serializer, JLDRelation.typeString, subtypeString, mention, countAttachmentMap) {
}

object JLDRelation {
  val typeString = "relation"

  def newJLDRelation(serializer: JLDSerializer, mention: EidosCrossSentenceEventMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): JLDRelation = {
    mention.odinMention.label match {
        // TODO: Figure out provenance for these!
//      case JLDRelationCorrelation.taxonomy => new JLDRelationCorrelation(serializer, mention, countAttachmentMap)
//      case JLDRelationCausation.taxonomy => new JLDRelationCausation(serializer, mention, countAttachmentMap)
      case JLDRelationMigration.taxonomy => new JLDRelationMigration(serializer, mention, countAttachmentMap)
      case _ => throw new IllegalArgumentException("Unknown CrossSentenceEventMention: " + mention)
    }
  }

  def newJLDRelation(serializer: JLDSerializer, mention: EidosEventMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): JLDRelation = {
    // This could be looked up in the taxonomy somehow, but the taxonomy doesn't include
    // information about the name of the arguments anyway, so may as well do all decoding here.
    // This could be pushed down to JLDRelation given EidosEventMention => JLDRelation.
    // If the JSON-LD specification doesn't change, then it is possible for the argument
    // names to be specified in master.yml file and then be taken over verbatim by querying the
    // arguments dictionary.
    mention.odinMention.label match {
      case JLDRelationCorrelation.taxonomy => new JLDRelationCorrelation(serializer, mention, countAttachmentMap)
      case JLDRelationCausation.taxonomy => new JLDRelationCausation(serializer, mention, countAttachmentMap)
      case JLDRelationMigration.taxonomy => new JLDRelationMigration(serializer, mention, countAttachmentMap)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + mention)
    }
  }

  def newJLDRelation(serializer: JLDSerializer, mention: EidosCrossSentenceMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): JLDRelation = {
    // Cross sentence mentions are always a coreference.
    if (JLDRelationCoreference.taxonomy == mention.odinMention.label)
      new JLDRelationCoreference(serializer, mention, countAttachmentMap)
    else
      throw new IllegalArgumentException("Unknown Mention: " + mention)
  }
}

class JLDRelationCausation(serializer: JLDSerializer, mention: EidosEventMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
    extends JLDRelation(serializer, JLDRelationCausation.subtypeString, mention, countAttachmentMap) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationCausation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCausation.effect, Seq.empty).filter(isExtractable)
//    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    sources ++ targets /*++ triggers*/ ++ super.getMentions
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

class JLDRelationCorrelation(serializer: JLDSerializer, mention: EidosEventMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
  extends JLDRelation(serializer, JLDRelationCorrelation.subtypeString, mention, countAttachmentMap) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationCorrelation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCorrelation.effect, Seq.empty).filter(isExtractable)
//    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    sources ++ targets /*++ triggers*/ ++ super.getMentions
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

class JLDRelationCoreference(serializer: JLDSerializer, mention: EidosCrossSentenceMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
  extends JLDRelation(serializer, JLDRelationCoreference.subtypeString, mention, countAttachmentMap) {

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

class JLDRelationMigration(serializer: JLDSerializer, mention: EidosEventMention, countAttachmentMap: Map[CountAttachment, JLDCountAttachment])
    extends JLDRelation(serializer, JLDRelationMigration.subtypeString, mention, countAttachmentMap) {

  override def getMentions: Seq[EidosMention] = {
    val mentions: Seq[EidosMention] = JLDRelationMigration.keys.flatMap { key =>
      mention.eidosArguments.getOrElse(key, Seq.empty[EidosMention])
    }

    mentions ++ super.getMentions
  }

  // The provenance of this mention is just that of anchor and neighbor.
//  override protected def provenance(): Seq[JValue] = Seq(
//    new JLDProvenance(serializer, mention.)
//    new JLDProvenance(serializer, mention.eidosAnchor).toJObject,
//    new JLDProvenance(serializer, mention.eidosNeighbor).toJObject
//  )

  override def toJObject: TidyJObject = {
    val trigger = new JLDTrigger(serializer, mention.eidosTrigger).toJObject
    val keysAndValues = JLDRelationMigration.keys.flatMap { key =>
      val values = mention.eidosArguments.getOrElse(key, Seq.empty[EidosMention])

      values.map { value =>
        (key, value)
      }
    }
    val jldArguments = keysAndValues.map { case (key, value: EidosMention) =>
      new JLDArgument(serializer, key, value).toJObject
    }

    super.toJObject + TidyJObject(List(
      JLDTrigger.singular -> trigger,
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationMigration {
  val subtypeString = "migration"
  val taxonomy = "HumanMigration"
  val keys: Seq[String] = Seq("group", "groupModifier", "moveTo", "moveFrom", "moveThrough", "timeStart", "timeEnd", "time")
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
    // This used to use the raw text and not show the processed word.  However, that does not work well
    // when we round-trip the data, because the conversion from raw to processed does not take place then.
    // val jldText: Option[String] = document.text.map(text => text.substring(startOffset, endOffset))
    val jldText: Option[String] = Some(sentence.words(index))

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

class JLDTimeInterval(serializer:JLDSerializer, val start: LocalDateTime, val end: LocalDateTime)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDTimeInterval.typename) {
  
  override def toJObject: TidyJObject = {
    val startDateTime = start.toString
    val endDateTime = end.toString

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "start" -> startDateTime,
      "end" -> endDateTime
    ))
  }
}

object JLDTimeInterval {
  val singular = "interval"
  val plural = "intervals"
  val typename = "TimeInterval"
}


class JLDTimex(serializer:JLDSerializer, val timex: TimEx)
    // The document, sentence, index above will be used to recognized words.
    extends JLDObject(serializer, JLDTimex.typename, timex) {
  
  override def toJObject: TidyJObject = {
    val jldIntervals = timex.intervals.map(interval => new JLDTimeInterval(serializer, interval.startDate, interval.endDate).toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "startOffset" -> timex.span.start,
      "endOffset" -> timex.span.end,
      "text" -> timex.text,
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
    "geoID" -> geoid.geonameID
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

class JLDSentence(serializer: JLDSerializer, document: Document, sentence: Sentence, timExs: Seq[TimEx],
    geoPhraseIDs: Seq[GeoPhraseID], jldCountAttachments: Seq[JLDCountAttachment])
    extends JLDObject(serializer, JLDSentence.typename, sentence)
{

  protected def getSentenceText(sentence: Sentence): String = getSentenceFragmentText(sentence, 0, sentence.words.length)

  // This and the one above are copied almost verbatim from Sentence.scala.  We can't readily
  // use the version with the raw text because when the words are read back in, the conversion
  // is not performed again.
  protected def getSentenceFragmentText(sentence: Sentence, start: Int, end: Int): String = {
    if (end - start == 1) return sentence.raw(start)

    val text = new mutable.StringBuilder()
    for (i <- start until end) {
      if(i > start) {
        // add as many white spaces as recorded between tokens
        val numberOfSpaces = math.max(1, sentence.startOffsets(i) - sentence.endOffsets(i - 1))

        0.until(numberOfSpaces).foreach { _ => text.append(" ") }
      }
      text.append(sentence.words(i)) // Changed from raw
    }
    text.toString()
  }

  override def toJObject: TidyJObject = {
    val key = GraphMap.UNIVERSAL_ENHANCED
    val jldWords = sentence.words.indices.map(new JLDWord(serializer, document, sentence, _))
    val dependencies = sentence.graphs.get(key)
//    val sent_id = document.sentences.indexOf(sentence)
    val timexes: Seq[JObject] = timExs.map { timEx =>
      new JLDTimex(serializer, timEx).toJObject
    }
    val geoExps: Seq[JObject] = geoPhraseIDs.map { geoPhraseID =>
      new JLDGeoID(serializer, geoPhraseID).toJObject
    }
    // This is given access to the words because they are nicely in order and no searching need be done.
    val jldGraphMapPair = dependencies.map(dependency => new JLDGraphMapPair(serializer, key, dependency, jldWords).toJValue)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "text" -> getSentenceText(sentence),
      JLDWord.plural -> jldWords.map(_.toJObject),
      JLDDependency.plural -> jldGraphMapPair,
      JLDTimex.plural -> timexes,
      JLDGeoID.plural -> geoExps,
      JLDCountAttachment.plural -> jldCountAttachments.map(_.toJObject)
    ))
  }
}

object JLDSentence {
  val singular = "sentence"
  val plural = "sentences"
  val typename = "Sentence"
}

class JLDDocument(serializer: JLDSerializer, annotatedDocument: AnnotatedDocument, jldCountAttachments: Seq[JLDCountAttachment])
    extends JLDObject(serializer, JLDDocument.typename, annotatedDocument.document) {

  protected def alignJldCountAttachments(jldCountAttachments: Seq[JLDCountAttachment], sentences: Array[Sentence]): Array[Seq[JLDCountAttachment]]= {
    val alignedCountAttachments: Array[Seq[JLDCountAttachment]] = sentences.map { sentence =>
      val sentenceStart = sentence.startOffsets.head
      val sentenceEnd = sentence.endOffsets.last

      jldCountAttachments.filter { jldCountAttachment =>
        val countAttachment = jldCountAttachment.countAttachment

        sentenceStart <= countAttachment.startOffset && countAttachment.endOffset <= sentenceEnd
      }
    }
    alignedCountAttachments
  }

  override def toJObject: TidyJObject = {
    val sentences = annotatedDocument.document.sentences
    val timExs = TimeNormFinder.getTimExs(annotatedDocument.odinMentions, sentences)
    val geoPhraseIDs = GeoNormFinder.getGeoPhraseIDs(annotatedDocument.odinMentions, sentences)
    val alignedJldCountAttachments = alignJldCountAttachments(jldCountAttachments, sentences)

    val jldSentences = sentences.zipWithIndex.map { case (sentence, index) =>
      new JLDSentence(serializer, annotatedDocument.document, sentence, timExs(index), geoPhraseIDs(index), alignedJldCountAttachments(index)).toJObject
    }.toSeq
    val jldText = annotatedDocument.document.text.map(text => text)
    val dctOpt = DctDocumentAttachment.getDct(annotatedDocument.document)
    val jldDCT = dctOpt.map(dct => new JLDDCT(serializer, dct).toJObject)

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "id" -> annotatedDocument.document.id,
      "title" -> TitleDocumentAttachment.getTitle(annotatedDocument.document),
      "location" -> LocationDocumentAttachment.getLocation(annotatedDocument.document),
      JLDDCT.singular -> jldDCT,
      "text" -> jldText,
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

  protected def collectMentions(mentions: Seq[EidosMention], mapOfMentions: JIdentityHashMap[EidosMention, Int],
      countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): Seq[JLDExtraction] = {
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
      val jldExtraction = newJLDExtraction(mention, countAttachmentMap)
      val recMentions = jldExtraction.getMentions
      val jldExtractions = jldExtraction +: collectMentions(recMentions, mapOfMentions, countAttachmentMap)

      jldExtractions
    }
  }

  protected def collectMentions(mentions: Seq[EidosMention], countAttachmentMap: Map[CountAttachment, JLDCountAttachment]): Seq[JLDExtraction] = {
    val mapOfMentions = new JIdentityHashMap[EidosMention, Int]()

    collectMentions(mentions, mapOfMentions, countAttachmentMap)
  }


  case class SortRecord(document: Document, documentIndex: Int, jldExtraction: JLDExtraction, mention: Mention)

  protected def compare(leftProvenances: Seq[Provenance], rightProvenances: Seq[Provenance]): Int = {
    if (leftProvenances.isEmpty != rightProvenances.isEmpty)
      if (leftProvenances.isEmpty) -1
      else +1
    else {
      if (leftProvenances.isEmpty) 0 // They both are.
      else {
        val leftProvenance = leftProvenances.head
        val rightProvenance = rightProvenances.head
        val provenanceComparison = leftProvenance.compareTo(rightProvenance)

        if (provenanceComparison != 0)
          provenanceComparison
        else
          compare(leftProvenances.tail, rightProvenances.tail)
      }
    }
  }

  def lessThan(mapOfDocuments: JIdentityHashMap[Document, Int])(left: JLDExtraction, right: JLDExtraction): Boolean = {
    val ordering = Array(
      JLDConceptEntity.subtypeString,
      JLDRelationCausation.subtypeString,
      JLDRelationCorrelation.subtypeString,
      JLDRelationCoreference.subtypeString
    )
    val leftOdinMention = left.eidosMention.odinMention
    val rightOdinMention = right.eidosMention.odinMention
    val leftDocumentIndex = mapOfDocuments.get(leftOdinMention.document)
    val rightDocumentIndex = mapOfDocuments.get(rightOdinMention.document)

    def breakTie(): Boolean = {
      // Really this should visit anything in Mention.equals, but many aren't obvious to the jsonld reader.
      // Instead, check the canonical text, which might differ because of rule differences and then
      // the label, which should also be different.  Don't go so far as to check the arguments just yet.
      val leftCanonicalName = left.eidosMention.canonicalName
      val rightCanonicalName = right.eidosMention.canonicalName

      if (leftCanonicalName != rightCanonicalName)
        leftCanonicalName < rightCanonicalName
      else {
        val leftLabel = leftOdinMention.label
        val rightLabel = rightOdinMention.label

        if (leftLabel != rightLabel)
          leftLabel < rightLabel
        else
          if (leftOdinMention.attachments.size != rightOdinMention.attachments.size)
            leftOdinMention.attachments.size < rightOdinMention.attachments.size
          else {
            def mkAttachmentNames(odinMention: Mention) = odinMention.attachments.toList
                .map(_.getClass.getName).sorted.mkString(" ")
            val leftAttachmentNames = mkAttachmentNames(leftOdinMention)
            val rightAttachmentNames = mkAttachmentNames(rightOdinMention)

            if (leftAttachmentNames != rightAttachmentNames)
              leftAttachmentNames < rightAttachmentNames
            else
              true // Tie goes in favor of the left just because it came first.
        }
      }
    }

    if (leftDocumentIndex != rightDocumentIndex)
      leftDocumentIndex < rightDocumentIndex
    else {
      val provenanceComparison = Provenance(leftOdinMention).compareTo(Provenance(rightOdinMention))

      if (provenanceComparison != 0)
        provenanceComparison < 0
      else {
        val leftProvenances = left.getMentions.map { eidosMention => Provenance(eidosMention.odinMention) }.sorted
        val rightProvenances = right.getMentions.map { eidosMention => Provenance(eidosMention.odinMention) }.sorted
        val provenanceComparison = compare(leftProvenances, rightProvenances)

        if (provenanceComparison != 0)
          provenanceComparison < 0
        else {
          val leftOrdering = ordering.indexOf(left.subtypeString)
          val rightOrdering = ordering.indexOf(right.subtypeString)

          if (leftOrdering != rightOrdering)
            leftOrdering < rightOrdering
          else {
            if ((leftOrdering == 1 && rightOrdering == 1) || (leftOrdering == 2 && rightOrdering == 2)) {
              val leftTrigger = leftOdinMention.asInstanceOf[EventMention].trigger
              val rightTrigger = rightOdinMention.asInstanceOf[EventMention].trigger
              val leftProvenance = Provenance(leftTrigger)
              val rightProvenance = Provenance(rightTrigger)
              val provenanceComparison = leftProvenance.compareTo(rightProvenance)

              if (provenanceComparison != 0)
                provenanceComparison < 0
              else
                breakTie()
            }
            else
              breakTie()
          }
        }
      }
    }
  }

  protected def sortJldExtractions(jldExtractions: Seq[JLDExtraction], corpus: Corpus): Seq[JLDExtraction] = {
    val mapOfDocuments = new JIdentityHashMap[Document, Int]()
    corpus.foreach { annotatedDocument =>
      mapOfDocuments.put(annotatedDocument.document, mapOfDocuments.size() + 1)
    }

    val sortedJldExtractions = jldExtractions.sortWith(lessThan(mapOfDocuments))

    // This is definitely the hack!  Make so that extractions are numbered nicely 1..n,
    // even though they have been discovered in a different order.
    serializer.reorder(sortedJldExtractions)
    sortedJldExtractions
  }

  protected def getCountAttachments(odinMentions: Seq[Mention]): Seq[CountAttachment]= {
    val reachableMentions = EidosMention.findReachableMentions(odinMentions)
    val countAttachmentSeq: Seq[CountAttachment] = reachableMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: CountAttachment => attachment
      }
    }
    // Do not use identity here, but equals, because we will base this on equality when outputting the mentions.
    //    val countAttachmentMap: IdentityHashMap[CountAttachment, Int] = countAttachmentSeq.foldLeft(new IdentityHashMap[CountAttachment, Int]()) { (identityHashMap, countAttachment) =>
    //      identityHashMap.put(countAttachment, 0)
    //      identityHashMap
    //    }
    val countAttachmentMap = countAttachmentSeq.map { countAttachment => countAttachment -> 0 }.toMap
    val countAttachmentArray = countAttachmentMap
        .keySet
        //        .asScala
        .toArray
        .sortWith { (left: CountAttachment, right: CountAttachment) =>
          if (left.startOffset != right.startOffset)
            left.startOffset < right.startOffset
          else if (left.endOffset != right.endOffset)
            left.endOffset < right.endOffset
          else
            true
        }

    countAttachmentArray.toSeq
  }

  override def toJObject: TidyJObject = {
    // These are then on a per document basis.
    val countAttachmentsSeq: Seq[Seq[CountAttachment]] =
        corpus.map { annotatedDocument => getCountAttachments(annotatedDocument.odinMentions) }
    val jldCountAttachmentsSeq: Seq[Seq[JLDCountAttachment]] =
        countAttachmentsSeq.map { countAttachments => countAttachments.map { countAttachment => new JLDCountAttachment(serializer, countAttachment) } }
    val countAttachmentMap: Map[CountAttachment, JLDCountAttachment] = countAttachmentsSeq.flatten.zip(jldCountAttachmentsSeq.flatten).toMap
    // TODO: Some of this seems to assume a single document, especially if attributes are compared by equals.
    val jldDocuments = corpus.zip(jldCountAttachmentsSeq).map { case (annotatedDocument, jldCountAttachments) => new JLDDocument(serializer, annotatedDocument, jldCountAttachments ) }
    val jldDocumentObjects = jldDocuments.map(_.toJObject) // So that serializer has typenames
    val exposedEidosMentions = corpus.flatMap(_.eidosMentions)
    val allJldExtractions = collectMentions(exposedEidosMentions, countAttachmentMap)
    val sortedJldExtractions = sortJldExtractions(allJldExtractions, corpus)
    val tidiedJldExtractions = sortedJldExtractions.map(_.toJObject)

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
      JLDDocument.plural -> jldDocumentObjects,
      JLDExtraction.plural -> tidiedJldExtractions
    ))
  }
}

object JLDCorpus {
  val singular = "corpus"
  val plural = "corpora"
  val typename = "Corpus"
}
