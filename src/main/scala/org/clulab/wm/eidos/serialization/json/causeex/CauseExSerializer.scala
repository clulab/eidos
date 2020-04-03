package org.clulab.wm.eidos.serialization.json.causeex

import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosEventMention

import org.json4s
import org.json4s.JsonDSL._
import org.json4s._

// This is the base class.

abstract class CauseExObject {
  val optionalUnknown = JNull
  val nonOptionalConfidence = JDouble(1d)
  val optionalConfidence: JValue = optionalUnknown
  val emptyMap = JObject()

  def toJValue(): JValue

  implicit def toJValue(causalAssertionObject: CauseExObject): JValue = causalAssertionObject.toJValue()
}

// These classes are described directly in the documentation in this order.

// Frame objects are used to represent events and causal assertions.
class Frame(eidosEventMention: EidosEventMention) extends CauseExObject {

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
class Entity(something: AnyRef) extends CauseExObject {

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
class Argument extends CauseExObject {

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

class Span(something: AnyRef) extends CauseExObject {

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

class CausalFactor(eidosEventMention: EidosEventMention) extends CauseExObject {

  def toJValue(): JObject = {
    TidyJObject(
      "factor_class" -> null, // Ontologized URI string for causal factor class
      "relevance" -> null, // Not optional, must be 0.0 to 1.0
      "magnitude" -> null, // Not optional, must be -1.0 to 1.0
      "trend" -> null // DECREASING, NEUTRAL, INCREASING, UNKNOWN
    )
  }
}

class Provenance(something: AnyRef) extends CauseExObject {

  def toJValue(): JObject = {
    TidyJObject(
      "source_team" -> new JArray(List("Lum AI"))
    )
  }
}

// These are the classes for internal use, generally described bottom up.

class EntityProperties(eidosEventMention: EidosEventMention) extends CauseExObject {

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

class FrameProperties(eidosEventMention: EidosEventMention) extends CauseExObject {

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

class Arguments(eidosEventMention: EidosEventMention) extends CauseExObject {

  def toJValue(): JArray = {
    val causes = eidosEventMention.eidosArguments("cause")
    val effects = eidosEventMention.eidosArguments("effect")
    ???
  }
}

class CausalFactors(eidosEventMention: EidosEventMention) extends CauseExObject {

  def toJValue(): JArray = {
    ???
  }
}

class EntityEvidence(eidosEventMention: EidosEventMention) extends CauseExObject {

  def toJValue(): JObject = {
    TidyJObject(
      "span" -> new Span(null), // Optional
      "head_span" -> new Span(null)  // Optional
    )
  }
}

class FrameEvidence(eidosEventMention: EidosEventMention) extends CauseExObject {

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

class CauseExDocument(annotatedDocument: AnnotatedDocument) extends CauseExObject {

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
