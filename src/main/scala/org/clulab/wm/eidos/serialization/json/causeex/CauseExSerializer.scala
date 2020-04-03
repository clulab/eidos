package org.clulab.wm.eidos.serialization.json.causeex

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosEventMention
import org.clulab.wm.eidos.mentions.EidosMention
import org.json4s
import org.json4s.JsonDSL._
import org.json4s._

// This is the base class/trait.

trait CauseExObject {
  def toJValue(): JValue

  implicit def toJValue(causalAssertionObject: CauseExObject): JValue = causalAssertionObject.toJValue()

  def getDocumentId(eidosMention: EidosMention): Option[String] = getDocumentId(eidosMention.odinMention)

  def getDocumentId(odinMention: Mention): Option[String] = getDocumentId(odinMention.document)

  def getDocumentId(document: Document): Option[String] = document.id
}

trait CauseExField {
  def toJField(): JField
}

object CauseExObject {
  // Handy singleton values are here.
  val optionalUnknown: JValue = JNull
  val nonOptionalConfidence: JValue = JDouble(1d)
  val optionalConfidence: JValue = optionalUnknown
  val emptyJObject: JObject = JObject()
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

  def isInEventOntology(uri: String): Boolean = uri.startsWith("http://ontology.causeex.com/ontology/odps/Event#")

  def newFrameTypes(condition: EidosMention => Boolean, uri: String): Seq[FrameType] =
      if (condition(eidosMention)) Seq(new FrameType(uri))
      else Seq.empty

  def toJValue(): JObject = {
    val frameTypes = Seq(
      newFrameTypes(isCausalAssertion,  "http://ontology.causeex.com/ontology/odps/CauseEffect#CausalAssertion"),
      newFrameTypes(isQualifiedEvent,   "http://ontology.causeex.com/ontology/odps/CauseEffect#QualifiedEvent"),
      newFrameTypes(isSimilarAssertion, "http://ontology.causeex.com/ontology/odps/CauseEffect#SimilarAssertion"),
      // A better way is to use a special ontology like two_six_events here.
      eidosMention.grounding.get("two_six").map { grounding =>
        // This is defensive here in case there are other kinds of groundings.
        val eventGroundings = grounding.grounding.filter { case (namer, _) =>
          isInEventOntology(namer.name)
        }

        eventGroundings.map { case (namer, float) =>
          new FrameType(namer.name, float)
        }
      }.getOrElse(Seq.empty)
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
      "docid" -> getDocumentId(eidosMention),
      "provenance" -> new Provenance(eidosMention)
    )
  }
}

// Check first with the arguments further below.
class Entity(something: AnyRef) extends CauseExObject {

  def toJValue(): JObject = {
    TidyJObject(
      // Ontologized URI string for entity type
      // Value represents non-optional confidence
      "entity_types" -> CauseExObject.nonOptionalConfidence,
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
      "confidence" -> CauseExObject.optionalConfidence,
      // Exactly one of entity, frame, or span must be specified
      "entitiy" -> new Entity(null), // Optional
      "frame" -> new Frame(null), // Optional
      "span" -> new Span(null) // Optional, only valid with the role "has_time"
    )
  }
}

class EntityArgument extends Argument {

}

class FrameArgument extends Argument {

}

class TimeArgument(time: Time) extends Argument {

  def toJValue(): JObject = {
    TidyJObject(
      "role" -> "has_time",
      "confidence" -> CauseExObject.optionalConfidence,
      "span" -> new SimpleTime(time.interval)
    )
  }
}

class Span(docId: String, start: Int, length: Int, text: String) extends CauseExObject {

  def this(odinMention: Mention) = this(
    getDocumentId(odinMention).get,
    odinMention.tokenInterval.start,
    odinMention.tokenInterval.end - odinMention.tokenInterval.start,
    odinMention.text
  )

  def this(eidosMention: EidosMention) = this(eidosMention.odinMention)

  def toJValue(): JObject = {
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

class FrameType(uri: String, confidence: Float = 1f) extends CauseExField {

  def toJField(): JField = JField(uri, JDouble(confidence))
}

class FrameTypes(frameTypes: Seq[FrameType]) extends CauseExObject {

  def toJValue(): JObject = new JObject(frameTypes.map(_.toJField).toList)
}

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

class Modality(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Asserted
  // http://ontology.causeex.com/ontology/odps/Event#Other

  def toJValue(): JValue = JString("http://ontology.causeex.com/ontology/odps/Event#Asserted")
}

class Polarity(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Negative
  // http://ontology.causeex.com/ontology/odps/Event#Positive

  def isNegative(): Boolean =
      eidosMention.odinMention.attachments.exists { attachment =>
        attachment.isInstanceOf[Negation]
      }

  def toJValue(): JValue =
      if (isNegative())
        JString("http://ontology.causeex.com/ontology/odps/Event#Negative")
      else
        JString("http://ontology.causeex.com/ontology/odps/Event#Positive")
}

class Tense(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Future
  // http://ontology.causeex.com/ontology/odps/Event#Past
  // http://ontology.causeex.com/ontology/odps/Event#Present

  // TODO: Try to figure out the verb tense or use timenorm information.
  def toJValue(): JValue = JNothing
}

class Genericity(eidosMention: EidosMention) extends CauseExObject {
  // http://ontology.causeex.com/ontology/odps/Event#Generic
  // http://ontology.causeex.com/ontology/odps/Event#Specific

  // TODO: Figure this out.
  def toJValue(): JValue = JNothing
}

class FrameProperties(eidosMention: EidosMention) extends CauseExObject {

  // Not optional, but may be an empty map
  def toJValue(): JObject = {
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

  def toCauseJValue(eidosMention: EidosMention): JObject = {
    new JObject(List.empty)
  }

  def toEffectJValue(eidosMention: EidosMention): JObject = {
    new JObject(List.empty)
  }

  def toJValue(): JArray = {
    val argumentsJValue = eidosMention.eidosArguments.map { case (key, eidosMentions) =>
      key match {
        case "cause" => toCauseJValue(eidosMention)
        case "effect" => toEffectJValue(eidosMention)
        case _ => JNull
      }
    }

//    new JArray(argumentsJValue)
    new JArray(List.empty[JValue])
  }
}

class CausalFactors(eidosMention: EidosMention) extends CauseExObject {
  // TODO: Add the ICM stuff here, perhaps mapped from groundings?

//  def icmGroundings(): MultiOntologyGrounding = {
//    eidosMention.grounding("two_six_icm").grounding.flatMap { case (namer, value) =>
//
//    }
//  }

  def getTimes: Seq[Time] = eidosMention.odinMention.attachments.toSeq.collect { case time: Time => time }

  def toJValue(): JArray = {

    // check first about has_time from the attributes of the mention
//    eidosMention.groun
null
  }
}

class EntityEvidence(eidosMention: EidosMention) extends CauseExObject {

  def toJValue(): JObject = {
    TidyJObject(
      "span" -> new Span(null), // Optional
      "head_span" -> new Span(null)  // Optional
    )
  }
}

abstract class Trigger(val eidosEventMention: EidosEventMention) extends CauseExObject {
  val eidosTrigger = eidosEventMention.eidosTrigger
  val odinTrigger = eidosEventMention.odinTrigger
  val text = odinTrigger.text

  def wordCount(text: String): Int = text.count(_ == ' ') + 1
}

class ContractedTrigger(eidosEventMention: EidosEventMention) extends Trigger(eidosEventMention) {

  def toJValue(): JObject = {

      if (wordCount(text) == 1) new Span(eidosTrigger).toJValue
        // The canonicalName does not have "unnormalized text referred to by the start/length".
      // else if (wordCount(eidosTrigger.canonicalName) == 1) new Span(eidosTrigger.canonicalName).toJValue
      else CauseExObject.emptyJObject
  }
}

class ExtendedTrigger(eidosEventMention: EidosEventMention) extends Trigger(eidosEventMention) {

  def toJValue(): JObject = {
      if (wordCount(text) > 1) new Span(eidosTrigger).toJValue
      else CauseExObject.emptyJObject
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

  def toJValue(): JObject = {
    eidosMention match {
      case eidosEventMention: EidosEventMention => toJValue(eidosEventMention)
      case _ => toJValue(eidosMention)
    }
  }
}

class CauseExDocument(annotatedDocument: AnnotatedDocument) extends CauseExObject {

  def toJValue(): JArray = {
    val framesJValue = annotatedDocument.eidosMentions
        .map { eidosMention =>
          new Frame(eidosMention).toJValue()
        }
        .toList

    new JArray(framesJValue)
  }
}
