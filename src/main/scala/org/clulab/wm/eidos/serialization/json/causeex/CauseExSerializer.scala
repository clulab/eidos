package org.clulab.wm.eidos.serialization.json.causeex

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import scala.language.implicitConversions

import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases.SingleOntologyGrounding
import org.clulab.wm.eidos.mentions.EidosEventMention
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.JsonDSL._
import org.json4s._

// This is the base class/trait.

trait CauseExObject {
  def toJValue: JValue

  implicit def toJValue(causalAssertionObject: CauseExObject): JValue = causalAssertionObject.toJValue
}

trait CauseExField {
  def toJField: JField
}

object CauseExObject {
  // Handy singleton values are here.
  val optionalUnknown: JValue = JNull
  val unknownDocumentId = "<unknown>" // This is only used if a require is commented out.

  val optionalConfidence: JValue = optionalUnknown
  val requiredConfidence: JValue = JDouble(1d) // also called non-optional

  def getDocumentId(eidosMention: EidosMention): String = getDocumentId(eidosMention.odinMention)

  def getDocumentId(odinMention: Mention): String = getDocumentId(odinMention.document)

  def getDocumentId(document: Document): String = document.id.getOrElse(unknownDocumentId)

  def toShortName(name: String): String = {
    val leafName =
      if (name.last == '/') name.dropRight(1)
      else name
    val shortName = StringUtils.afterLast(leafName, '/')

    shortName
  }

  def getSingleOntologyGroundings(eidosMention: EidosMention, key: String): Seq[SingleOntologyGrounding] = {
    val singleOntologyGroundings = eidosMention.grounding.get(key).map { ontologyGrounding =>
      ontologyGrounding.grounding
    }.getOrElse(Seq.empty)
    // This is necessary because the same leaf can have multiple parents.
    // Unfortunately there is no distinctBy in this version of Scala.
    val distinctOntologyGroundings = singleOntologyGroundings
        .groupBy { singleOntologyGrounding => toShortName(singleOntologyGrounding._1.name) }
        .map { case (name, singleOntologyGroundings) => name -> singleOntologyGroundings.head }
        .toSeq
        .sortBy { case (key, (_, float)) => (-float, key) }
        .map { case (_, singleOntologyGrounding) => singleOntologyGrounding }

    distinctOntologyGroundings
  }
}

// These classes are described directly in the documentation in this order.

// Frame objects are used to represent events and causal assertions.
class Frame(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/CauseEffect#CausalAssertion
  // http://ontology.causeex.com/ontology/odps/CauseEffect#QualifiedEvent
  // http://ontology.causeex.com/ontology/odps/CauseEffect#SimilarAssertion
  // ...anything from the event ontology hierarchy which could come from groundings...
  // http://ontology.causeex.com/ontology/odps/Event#AbductionHostageTakingOrHijacking
  // ...
  // http://ontology.causeex.com/ontology/odps/Event#Webcast

  def isCausalAssertion(eidosMention: EidosMention): Boolean =
      eidosMention.eidosArguments.contains("cause") && eidosMention.eidosArguments.contains("effect")

  def isQualifiedEvent(eidosMention: EidosMention): Boolean = false

  // TODO Could this be coreference?
  def isSimilarAssertion(eidosMention: EidosMention): Boolean = false

  def isFrameType(eidosMention: EidosMention)(singleOntologyGrounding: SingleOntologyGrounding): Boolean = {
    0.5 < singleOntologyGrounding._2
  }

  def newFrameTypes(condition: EidosMention => Boolean, uri: String): Seq[FrameType] =
      if (condition(eidosMention)) Seq(new FrameType(uri))
      else Seq.empty

  def toJValue: JObject = {
    val frameTypes = Seq(
      newFrameTypes(isCausalAssertion,  "http://ontology.causeex.com/ontology/odps/CauseEffect#CausalAssertion"),
      newFrameTypes(isQualifiedEvent,   "http://ontology.causeex.com/ontology/odps/CauseEffect#QualifiedEvent"),
      newFrameTypes(isSimilarAssertion, "http://ontology.causeex.com/ontology/odps/CauseEffect#SimilarAssertion"),
      // A better way is to use a special ontology like two_six_events here.
      CauseExObject.getSingleOntologyGroundings(eidosMention, "two_six")
          .filter(isFrameType(eidosMention)(_))
          .map(new FrameType(_, "http://ontology.causeex.com/ontology/odps/Event#"))
    ).flatten

    TidyJObject(
      "frame_types" -> new FrameTypes(frameTypes),
      // These will be the causes and effects.
      "causal_factors" -> new CausalFactors(eidosMention),
      // These will be the attributes of time and location.
      "arguments" -> new Arguments(eidosMention),
      // TODO Require this somehow so that it does not get tidied.
      // Not optional, but may be an empty map
      "properties" -> new FrameProperties(eidosMention),
      // TODO We do have possibly have polarity
      "evidence" -> new FrameEvidence(eidosMention),
      "docid" -> CauseExObject.getDocumentId(eidosMention),
      "provenance" -> new Provenance()
    )
  }
}

// Check first with the arguments further below.
class Entity(eidosMention: EidosMention) extends CauseExObject {
  // EntityTypes
  // http://ontology.causeex.com/ontology/odps/Actor#AdvocacyGroup
  //  ...
  // http://ontology.causeex.com/ontology/odps/GeneralConcepts#Wetland

  // MentionTypes
  // http://ontology.causeex.com/ontology/odps/DataProvenance#NAM
  // http://ontology.causeex.com/ontology/odps/DataProvenance#NOM
  // http://ontology.causeex.com/ontology/odps/DataProvenance#PRO

  def isEntityType(eidosMention: EidosMention)(singleOntologyGrounding: SingleOntologyGrounding): Boolean = true

  def toJValue: JObject = {
    val entityTypes = Seq(
      // A better way is to use a special ontology like two_six_events here.
      CauseExObject.getSingleOntologyGroundings(eidosMention, "two_six_actor")
          .filter(isEntityType(eidosMention)(_))
          // TODO: There are two different prefixes in play.  Are they in the same ontology?
          .map { singleOntologyGrounding => new EntityType(singleOntologyGrounding, "http://ontology.causeex.com/ontology/odps/Event#") }
    ).flatten
    // TODO: Figure out the NAM, NOM, PRO
    val newMentionTypeOpt: Option[String] = None

    TidyJObject(
      // Ontologized URI string for entity type
      // Value represents non-optional confidence
      "entity_types" -> new EntityTypes(entityTypes),
      "mention_type" -> newMentionTypeOpt, // Ontologized URI string for mention type
      // Not optional, but may be an empty map
      "evidence" -> new EntityEvidence(eidosMention),
      "properties" -> new EntityProperties(eidosMention),
      "provenance" -> new Provenance(),
      // Most canonical name the system can provide for the entity
      "canonical_label" -> eidosMention.canonicalName // Optional
    )
  }
}

abstract class Argument extends CauseExObject

class EntityArgument(roleUri: String, eidosMention: EidosMention) extends Argument {

  def toJValue: JObject = {
    TidyJObject(
      // Either an ontologized URI string for the role or 'has_time' for time arguments.
      "role" -> roleUri,
      "confidence" -> CauseExObject.optionalConfidence,
      // Exactly one of entity, frame, or span must be specified
      "entitiy" -> new Entity(eidosMention) // Optional
    )
  }
}

class FrameArgument(roleUri: String, eidosMention: EidosMention) extends Argument {

  def toJValue: JObject = {
    TidyJObject(
      // Either an ontologized URI string for the role or 'has_time' for time arguments.
      "role" -> roleUri,
      "confidence" -> CauseExObject.optionalConfidence,
      // Exactly one of entity, frame, or span must be specified
      "frame" -> new Frame(eidosMention), // Optional
    )
  }
}

class TimeArgument(time: Time, docId: String) extends Argument {

  // TODO: See CauseEffect file for definition of latency and other roles!
  def getLatencyOpt: Option[Long] = {
    val start = time.interval.intervals
        .map(_.startDate)
        .map { startDate => (startDate, ChronoUnit.MILLIS.between(TimeArgument.zero, startDate)) }
        .minBy(_._2)
        ._1
    val end = time.interval.intervals
        .map(_.endDate)
        .map { endDate => (endDate, ChronoUnit.MILLIS.between(TimeArgument.zero, endDate)) }
        .maxBy(_._2)
        ._1
    val milliseconds = ChronoUnit.MILLIS.between(start, end)
    val millisecondsOpt = if (milliseconds > 0) Some(milliseconds) else None

    millisecondsOpt
  }

  def toJValue: JObject = {
    TidyJObject(
      // Either an ontologized URI string for the role or 'has_time' for time arguments.
      "role" -> "has_time",
      "confidence" -> CauseExObject.optionalConfidence,
      "span" -> new Span(docId, time) // Optional, only valid with the role "has_time"
    )
  }
}

object TimeArgument {
  val zero: LocalDateTime = LocalDateTime.now
}

class Span(val docId: String, val start: Int, val end: Int, val text: String) extends CauseExObject {
  val length: Int = end - start

  def this(odinMention: Mention) = this(
    CauseExObject.getDocumentId(odinMention),
    Span.getStart(odinMention),
    Span.getEnd(odinMention),
    odinMention.text // kwa: this needs to be raw text?
  )

  def this(eidosMention: EidosMention) = this(eidosMention.odinMention)

  def this(docId: String, time: Time) = this (
    docId,
    time.interval.span.start,
    time.interval.span.end,
    time.interval.text
  )

  def toJValue: JObject = {
    TidyJObject(
      // Document ID string as given in CDR
      "docid" -> docId,
      "start" -> start,
      "length" -> length,
      // Unnormalized text referred to by the start/length
      "text" -> text // Optional
    )
  }
}

object Span {

  def getStart(odinMention: Mention): Int = {
    odinMention
        .sentenceObj
        .startOffsets(odinMention.tokenInterval.start)
  }

  def getEnd(odinMention: Mention): Int = {
    odinMention
        .sentenceObj
        .endOffsets(math.min(odinMention.tokenInterval.end, odinMention.sentenceObj.endOffsets.length) - 1)
  }
}

// TODO: How are these calculated?
class HeadSpan(docId: String, start: Int, end: Int, text: String) extends Span(docId, start, end, text) {

  def this(odinMention: Mention) = this(
    CauseExObject.getDocumentId(odinMention),
    odinMention.tokenInterval.start,
    odinMention.tokenInterval.end,
    odinMention.text
  )

  def this(eidosMention: EidosMention) = this(eidosMention.odinMention)
}

object Trend extends Enumeration {
  type Trend = Value

  val DECREASING, NEUTRAL, INCREASING, UNKNOWN = Value
}

class CausalFactor(singleOntologyGrounding: SingleOntologyGrounding, trend: Trend.Value = Trend.UNKNOWN) extends CauseExObject {
  val namer: Namer = singleOntologyGrounding._1
  val float: Float = singleOntologyGrounding._2
  val factorClass: String = OntologizedType.toUri(namer.name, "http://ontology.causeex.com/ontology/odps/ICM#")

  def toJValue: JObject = {
    TidyJObject(
      "factor_class" -> factorClass, // Ontologized URI string for causal factor class
      "relevance" -> JDouble(float), // Not optional, must be 0.0 to 1.0
      // TODO: We need better magnitude.
      "magnitude" -> JDouble(0d), // Not optional, must be -1.0 to 1.0
      "trend" -> trend.toString // DECREASING, NEUTRAL, INCREASING, UNKNOWN
    )
  }
}

class Provenance() extends CauseExObject {

  def toJValue: JObject = {
    TidyJObject(
      "source_team" -> new JArray(List("Lum AI"))
    )
  }
}

// These are the classes for internal use, generally described bottom up.

class OntologizedType(val uri: String, val confidence: Float) extends CauseExField {

  def this(singleOntologyGrounding: SingleOntologyGrounding, prefix: String) =
    this(OntologizedType.toUri(singleOntologyGrounding._1.name, prefix), singleOntologyGrounding._2)

  def toJField: JField = JField(uri, JDouble(confidence))

  override def equals(other: Any): Boolean = {
    this.getClass == other.getClass && {
      val that = other.asInstanceOf[OntologizedType]

      this.uri == that.uri
    }
  }

  override def hashCode: Int = uri.hashCode
}

object OntologizedType {

  def toUri(name: String, prefix: String): String = prefix + CauseExObject.toShortName(name)
}

class FrameType(uri: String, confidence: Float = 1f) extends OntologizedType(uri, confidence) {

  def this(singleOntologyGrounding: SingleOntologyGrounding, prefix: String) =
      this(OntologizedType.toUri(singleOntologyGrounding._1.name, prefix), singleOntologyGrounding._2)
}

class FrameTypes(frameTypes: Seq[FrameType]) extends CauseExObject {

  def toJValue: JObject = new JObject(frameTypes.map(_.toJField).toList)
}

class EntityProperties(eidosMention: EidosMention) extends CauseExObject {

  def getLocationOpt: Option[Int] = {
    val locations = eidosMention.odinMention.attachments.collect { case attachment: Location => attachment }
    assert(locations.size <= 1)

    locations.headOption.flatMap { location =>
      location.geoPhraseID.geonameID.flatMap { string =>
        try {
          Some(string.toInt)
        }
        catch {
          case _: NumberFormatException => None
        }
      }
    }
  }

  // Not optional, but may be an empty map
  def toJValue: JObject = {
    TidyJObject(
      // TODO: Is there anything for this available, like latitude and longitude?
      "external_uri" -> JNothing, // Ontologized URI string for entity, optional
      "geonames_id" -> null, // Optional, int id used by the GeoNames API and in urls
      "latitude" -> JNothing, // Optional
      "longitude" -> JNothing // Optional
    )(required = true)
  }
}

class Modality(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Asserted
  // http://ontology.causeex.com/ontology/odps/Event#Other

  def toJValue: JValue = JString("http://ontology.causeex.com/ontology/odps/Event#Asserted")
}

class Polarity(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Negative
  // http://ontology.causeex.com/ontology/odps/Event#Positive

  def isNegative: Boolean =
      eidosMention.odinMention.attachments.exists { attachment =>
        attachment.isInstanceOf[Negation]
      }

  def toJValue: JValue =
      if (isNegative)
        JString("http://ontology.causeex.com/ontology/odps/Event#Negative")
      else
        JString("http://ontology.causeex.com/ontology/odps/Event#Positive")
}

class Tense(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Future
  // http://ontology.causeex.com/ontology/odps/Event#Past
  // http://ontology.causeex.com/ontology/odps/Event#Present

  // TODO: Try to figure out the verb tense or use timenorm information.
  def toJValue: JValue = JNothing
}

class Genericity(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Generic
  // http://ontology.causeex.com/ontology/odps/Event#Specific

  // TODO: Figure this out.
  def toJValue: JValue = JNothing
}

class FrameProperties(eidosMention: EidosMention) extends CauseExObject {

  // Not optional, but may be an empty map
  def toJValue: JObject = {
    TidyJObject(
      "modality" -> new Modality(eidosMention),   // Ontologized URI string for modality, optional
      "polarity" -> new Polarity(eidosMention),   // Ontologized URI string for polarity, optional
      "tense" -> new Tense(eidosMention),   // Ontologized URI string for tense, optional
      "genericity" -> new Genericity(eidosMention),   // Ontologized URI string for genericity, optional
      "confidence" -> CauseExObject.optionalConfidence // Optional
    )(required = true)
  }
}

class Arguments(eidosMention: EidosMention) extends CauseExObject {

  def getTimes: Seq[Time] = eidosMention.odinMention.attachments.toSeq.collect { case time: Time => time }

  def getCauses: Seq[EidosMention] = eidosMention.eidosArguments.getOrElse("cause", Seq.empty)

  def getEffects: Seq[EidosMention] = eidosMention.eidosArguments.getOrElse("effect", Seq.empty)

  // TODO: For now, grounding matches, but some need to be identified by type of attachment or name of argument.
  def isRole(argument: String, eidosMention: EidosMention): Boolean = false

  def getEntityArgumentsFromArguments: Seq[EntityArgument] = {
    eidosMention.eidosArguments.flatMap { case (argument, eidosMentions) =>
      eidosMentions
          .filter { eidosMention => isRole(argument, eidosMention) }
          // TODO: The key needs to be mapped into a role.
          .map { eidosMention => new EntityArgument(argument, eidosMention) }
    }.toSeq
  }

  def getEntityArgumentsFromAttachments: Seq[EntityArgument] = {
    eidosMention.odinMention.attachments.toSeq.flatMap { attachment =>
      attachment match {
        // This very eidosMention has the location.
        case location: Location => Some(new EntityArgument("http://ontology.causeex.com/ontology/odps/GeneralConcepts#location", eidosMention))
        // TODO: Find more
        case _ => None
      }
    }
  }

  def getEntityArguments: Seq[EntityArgument] = {
    getEntityArgumentsFromArguments ++ getEntityArgumentsFromAttachments
  }

  def toJValue: JArray = {
    val arguments = Seq(
      getTimes.map { time => new TimeArgument(time, CauseExObject.getDocumentId(eidosMention)) },
      getCauses.map { cause => new FrameArgument("http://ontology.causeex.com/ontology/odps/CauseEffect#has_cause", cause) },
      getEffects.map { effect => new FrameArgument("http://ontology.causeex.com/ontology/odps/CauseEffect#has_effect", effect) },
      // TODO: Check for other relevant arguments and attributes?
      getEntityArguments
    ).flatten

    new JArray(arguments.map(_.toJValue).toList)
  }
}

class CausalFactors(eidosMention: EidosMention) extends CauseExObject {
  // TODO: Add the ICM stuff here, perhaps mapped from groundings?
  // http://ontology.causeex.com/ontology/odps/ICM#AbilityToAddressBasicNeeds
  // ...
  // http://ontology.causeex.com/ontology/odps/ICM#Weather

  def getTrend: Trend.Value = {
    // There's no technical reason that it could be both
    if (eidosMention.odinMention.attachments.exists { attachment => attachment.isInstanceOf[Increase]})
      Trend.INCREASING
    else if (eidosMention.odinMention.attachments.exists { attachment => attachment.isInstanceOf[Increase]})
      Trend.DECREASING
    // TODO: Which of these should be used?
    else if (false)
      Trend.NEUTRAL
    else
      Trend.UNKNOWN
  }

  def isCausalFactor(eidosMention: EidosMention)(singleOntologyGrounding: SingleOntologyGrounding): Boolean = true

  def toJValue: JArray = {
    val trend = getTrend
    // TODO: The attachments will have a trigger and that should influence magnitude.
    val causalFactors = CauseExObject.getSingleOntologyGroundings(eidosMention, "two_six_icm")
        .filter(isCausalFactor(eidosMention)(_))
        .map(new CausalFactor(_, trend))

    new JArray(causalFactors.toList.map(_.toJValue))
  }
}

class EntityType(uri: String, confidence: Float = 1f) extends OntologizedType(uri, confidence) {

  def this(singleOntologyGrounding: SingleOntologyGrounding, prefix: String) =
    this(OntologizedType.toUri(singleOntologyGrounding._1.name, prefix), singleOntologyGrounding._2)
}

class EntityEvidence(eidosMention: EidosMention) extends CauseExObject {

  def toJValue: JObject = {
    TidyJObject(
      "span" -> new Span(eidosMention), // Optional
      "head_span" -> new HeadSpan(eidosMention)  // Optional
    )
  }
}

class EntityTypes(entityTypes: Seq[EntityType]) extends CauseExObject {

  def toJValue: JObject = new JObject(entityTypes.map(_.toJField).toList)
}

abstract class Trigger(val eidosEventMention: EidosEventMention) extends CauseExObject {
  val eidosTrigger: EidosMention = eidosEventMention.eidosTrigger
  val odinTrigger: TextBoundMention = eidosEventMention.odinTrigger
  val text: String = odinTrigger.text

  def wordCount(text: String): Int = text.count(_ == ' ') + 1
}

class ContractedTrigger(eidosEventMention: EidosEventMention) extends Trigger(eidosEventMention) {

  def toJValue: JObject = {

      if (wordCount(text) == 1) new Span(eidosTrigger).toJValue
        // The canonicalName does not have "unnormalized text referred to by the start/length".
      // else if (wordCount(eidosTrigger.canonicalName) == 1) new Span(eidosTrigger.canonicalName).toJValue
      else TidyJObject.emptyJObject
  }
}

class ExtendedTrigger(eidosEventMention: EidosEventMention) extends Trigger(eidosEventMention) {

  def toJValue: JObject = {
      if (wordCount(text) > 1) new Span(eidosTrigger).toJValue
      else TidyJObject.emptyJObject
  }
}

class FrameEvidence(eidosMention: EidosMention) extends CauseExObject {

  def toJValue(eidosEventMention: EidosEventMention): JObject = {
    TidyJObject(
      "trigger" -> new ContractedTrigger(eidosEventMention),
      "extended_trigger" -> new ExtendedTrigger(eidosEventMention),
      "sentence" -> new Span(eidosEventMention)
    )
  }

  def toJValue(eidosMention: EidosMention): JObject = {
    TidyJObject(
      "sentence" -> new Span(eidosMention)
    )
  }

  def toJValue: JObject = {
    eidosMention match {
      case eidosEventMention: EidosEventMention => toJValue(eidosEventMention)
      case _ => toJValue(eidosMention)
    }
  }
}

class CauseExDocument(annotatedDocument: AnnotatedDocument) extends CauseExObject {
  require(annotatedDocument.document.id.isDefined)

  def toJValue: JArray = {
    val frameJValues = annotatedDocument.eidosMentions
        .map { eidosMention =>
          new Frame(eidosMention).toJValue
        }
        .toList

    new JArray(frameJValues)
  }
}

case class SimpleTime(start: LocalDateTime, end: LocalDateTime, duration: Option[Long])

object SimpleTime {
  protected val zero: LocalDateTime = LocalDateTime.now

  // TODO: Move this method to where it fits better.
  // Method to convert a time attachment to DateTimes
  def fromTime(time: Time): SimpleTime = {
    val start = time.interval.intervals
        .map(_.startDate)
        .map { startDate => (startDate, ChronoUnit.MILLIS.between(zero, startDate)) }
        .minBy(_._2)
        ._1
    val end = time.interval.intervals
        .map(_.endDate)
        .map { endDate => (endDate, ChronoUnit.MILLIS.between(zero, endDate)) }
        .maxBy(_._2)
        ._1
    val milliseconds = ChronoUnit.MILLIS.between(start, end)
    val millisecondsOpt = if (milliseconds > 0) Some(milliseconds) else None

    SimpleTime(start, end, millisecondsOpt)
  }
}
