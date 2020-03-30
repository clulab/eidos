package org.clulab.wm.eidos.serialization.json.causeex

import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit

import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosEventMention
import org.json4s
import org.json4s.JsonDSL._
import org.json4s._

// This is the base class.

abstract class CausalAssertionObject {
  val optionalUnknown = JNull
  val nonOptionalConfidence = JDouble(1d)
  val optionalConfidence: JValue = optionalUnknown
  val emptyMap = JObject()

  def toJValue(): JValue

  implicit def toJValue(causalAssertionObject: CausalAssertionObject): JValue = causalAssertionObject.toJValue()
}

// These classes are described directly in the documentation in this order.

// Frame objects are used to represent events and causal assertions.
class Frame(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def toJValue(): JObject = {
    val documentIdOpt = eidosEventMention.odinMention.document.id

    TidyJObject(
      "frame_types" -> TidyJObject(
        // Value represents a non-optional confidence.
        "http://ontology.causeex.com/ontology/odps/CauseEffect#CausalAssertion" -> nonOptionalConfidence
      ),
      // These will be the causes and effects.
      "causal_factors" -> new CausalFactors(eidosEventMention),
      // These will be the attributes of time and location.
      "arguments" -> new Arguments(eidosEventMention),
      // TODO Require this somehow so that it does not get tidied.
      // These
      // Not optional, but may be an empty map
      "properties" -> new FrameProperties(eidosEventMention),
      // TODO We do have possibly have polarity
      "evidence" -> new FrameEvidence(eidosEventMention),
      "docid" -> documentIdOpt.get,
      "provenance" -> new Provenance(null)
    )
  }
}

// Check first with the arguments further below.
class Entity(something: AnyRef) extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      // Ontologized URI string for entity type
      // Value represents non-optional confidence
      "entity_types" -> nonOptionalConfidence,
      "mention_type" -> null, // Ontologized URI string for mention type
      // Not optional, but may be an empty map
      "evidence" -> new EntityEvidence(null),
      "properties" -> new EntityProperties(null),
      "provenance" -> new Provenance(null),
      // Most canonical name the system can provide for the entity
      "canonical_label" -> null // Optional
    )
    ???
  }
}

// What are the arguments, are these attributes like time and location?
class Argument extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      // Either an ontologized URI string for the role or 'has_time' for time arguments.
      "role" -> "",
      "confidence" -> optionalConfidence,
      // Exactly one of entity, frame, or span must be specified
      "entitiy" -> new Entity(null), // Optional
      "frame" -> new Frame(null), // Optional
      "span" -> new Span(null) // Optional, only valid with the role "has_time"
    )
  }
}

class Span(something: AnyRef) extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      // Document ID string as given in CDR
      "docid" -> "",
      "start" -> 0,
      "length" -> 6,
      // Unnormalized text referred to by the start/length
      "text" -> "" // Optional
    )
  }
}

class CausalFactor(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      "factor_class" -> null, // Ontologized URI string for causal factor class
      "relevance" -> null, // Not optional, must be 0.0 to 1.0
      "magnitude" -> null, // Not optional, must be -1.0 to 1.0
      "trend" -> null // DECREASING, NEUTRAL, INCREASING, UNKNOWN
    )
  }
}

class Provenance(something: AnyRef) extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      "source_team" -> new JArray(List("Lum AI"))
    )
  }
}

// These are the classes for internal use, generally described bottom up.

@SerialVersionUID(1L)
case class SimpleTime(start: LocalDateTime, end: LocalDateTime, duration: Option[Long])

class EntityProperties(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  // Not optional, but may be an empty map
  def toJValue(): JObject = {
    TidyJObject(
      "external_uri" -> null, // Ontologized URI string for entity, optional
      "geonames_id" -> null, // Optional, int id used by the GeoNames API and in urls
      "latitude" -> 0d, // Optional
      "longitude" -> 0d // Optional
    )(required = true)
  }
}

class FrameProperties(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  // Not optional, but may be an empty map
  def toJValue(): JObject = {
    TidyJObject(
      "modality" -> optionalUnknown,   // Ontologized URI string for modality, optional
      "polarity" -> optionalUnknown,   // Ontologized URI string for polarity, optional
      "tense" -> optionalUnknown,   // Ontologized URI string for tense, optional
      "genericity" -> optionalUnknown,   // Ontologized URI string for genericity, optional
      "confidence" -> optionalConfidence // Optional
    )(required = true)
  }
}

class Arguments(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def toJValue(): JArray = {
    val causes = eidosEventMention.eidosArguments("cause")
    val effects = eidosEventMention.eidosArguments("effect")
    ???
  }
}

class CausalFactors(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def toJValue(): JArray = {
    ???
  }
}

class EntityEvidence(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def toJValue(): JObject = {
    TidyJObject(
      "span" -> new Span(null), // Optional
      "head_span" -> new Span(null)  // Optional
    )
  }
}

class FrameEvidence(eidosEventMention: EidosEventMention) extends CausalAssertionObject {

  def wordCount(text: String): Int = text.count(_ == ' ') + 1

  def toJValue(): JObject = {
    val eidosTrigger = eidosEventMention.eidosTrigger
    val odinTrigger = eidosTrigger.odinMention
    val triggerOpt =
      if (wordCount(odinTrigger.text) == 1) // This is more reliable
      Some(odinTrigger.text)
      else if (wordCount(eidosTrigger.canonicalName) == 1)
        Some(eidosTrigger.canonicalName) // This might be shorter.
      else
      None
    val extendedTriggerOpt =
      if (wordCount(odinTrigger.text) > 1) // This is more reliable
      Some(odinTrigger.text)
      else
      None
    val sentence = eidosEventMention.odinMention.text

    TidyJObject(
      "trigger" -> triggerOpt.get,
      "extended_trigger" -> extendedTriggerOpt.get,
      "sentence" -> sentence
    )
  }
}

class CausalAssertionDocument(annotatedDocument: AnnotatedDocument) extends CausalAssertionObject {

  def toJValue(): JArray = {
    val framesJValue = annotatedDocument
        .allEidosMentions
        .collect { case eidosEventMention: EidosEventMention => eidosEventMention }
        .filter { eidosEventMention =>
          val causesOpt = eidosEventMention.eidosArguments.get("cause")
          val effectsOpt = eidosEventMention.eidosArguments.get("effect")

          // This will include subclass EidosCrossSentenceEventMention.
          causesOpt.isDefined &&
              effectsOpt.isDefined &&
              // This makes sure it isn't a correlation rather than a cause/effect.
              eidosEventMention.label == "Causal" &&
              // Only include it if there is anything to output.
              (causesOpt.get.nonEmpty || effectsOpt.get.nonEmpty)
        }
        .map { eidosEventMention =>
          new Frame(eidosEventMention: EidosEventMention).toJValue()
        }
        .toList

    new JArray(framesJValue)
  }
}

object CausalAssertionSerializer {
  protected val zero = LocalDateTime.now

  // TODO: Move this method to where it fits better.
  // Method to convert a time attachment to DateTimes
  def timeToSimpleTime(time: Time): SimpleTime = {
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
