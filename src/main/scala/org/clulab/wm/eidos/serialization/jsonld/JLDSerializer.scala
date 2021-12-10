package org.clulab.wm.eidos.serialization.jsonld

import java.io.PrintWriter
import java.time.LocalDateTime
import java.util.{IdentityHashMap => JIdentityHashMap}
import java.util.{Set => JavaSet}
import org.clulab.odin.Attachment
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.stringify
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.document._
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.RelevanceDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.clulab.wm.eidos.groundings.grounders.{AdjectiveGrounding, PredicateTuple}
import org.clulab.wm.eidos.groundings.{OntologyGrounding, PredicateGrounding, OntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.{EidosCrossSentenceEventMention, EidosCrossSentenceMention, EidosEventMention, EidosMention, EidosTextBoundMention}
import org.clulab.wm.eidos.utils.Unordered
import org.clulab.wm.eidos.utils.Unordered.OrderingOrElseBy
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable
import scala.math.Ordering.Implicits._ // enable list ordering

trait Regrounding {
  def reToJObject: TidyJObject = toJObject(true)
  def toJObject: TidyJObject = toJObject(false)
  def toJObject(regrounding: Boolean): TidyJObject
}

// This is an object than when asked to convert itself a JSON object or value, converts
// itself in a way that conforms to the JSON-LD standard as well.
abstract class JLDObject(val serializer: JLDSerializer, val typename: String, val value: Any = new Object()) {
  serializer.register(this)

  def serialize(printWriter: PrintWriter, pretty: Boolean = true, regrounding: Boolean = false): Unit = {
    val jValue = serialize(regrounding)

    printWriter.println(stringify(jValue, pretty))
  }

  def serialize(regrounding: Boolean): JValue = serializer.serialize(this, regrounding)

  def serialize(): JValue = serializer.serialize(this, false)

  def toJsonStr: String =
      pretty(render(serialize()))

  def toJObject: TidyJObject

  def reToJObject: TidyJObject = toJObject

  def newJLDExtraction(mention: EidosMention): JLDExtraction = mention match {
//    case mention: EidosCrossSentenceEventMention => JLDRelation.newJLDRelation(serializer, mention, countAttachmentMap)
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
class JLDSerializer {
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

  def mkType(typename: String): JField = {
    typenames += typename
    "@type" -> typename
  }

  def reidentify(jldExtractions: Seq[JLDExtraction]): Unit = {
    if (jldExtractions.nonEmpty) {
      val idsByIdentity = idsByTypenameByIdentity(jldExtractions.head.typename)

      jldExtractions.zipWithIndex.foreach { case (jldExtraction, index) =>
        idsByIdentity.put(jldExtraction.eidosMention, index + 1)
      }
    }
  }

  def mkType(jldObject: JLDObject): JField = mkType(jldObject.typename)

  def mkContext(regrounding: Boolean): TidyJObject = {
    // The wiki turns <a id="Document"> into <a id="user-content-document">
    // but w3id.org is not set up to lowercase the document, so it is done here in code.
    def mkContext(name: String): JField = new JField(name, JLDSerializer.base + name.toLowerCase())

    val types: List[JField] = typenames
      .toList
      .filter { typename => !regrounding || JLDSerializer.regroundingTypenames.contains(typename) }
      .sorted
      .map(mkContext)

    TidyJObject(types)
  }

  def mkRef(identity: Any): TidyJObject = {
    val typename = Option(typenamesByIdentity.get(identity))
        .getOrElse(throw new Exception("Cannot make reference to unknown identity: " + identity))
    val id = idsByTypenameByIdentity(typename).get(identity)

    val field: JField = mkId(typename, id)

    TidyJObject(List(field))
  }

  def serialize(jldObjectProvider: JLDObject, regrounding: Boolean = false): JValue = {
    val jObject: TidyJObject =
        if (regrounding) jldObjectProvider.reToJObject
        else jldObjectProvider.toJObject

    TidyJObject(List(
      "@context" -> mkContext(regrounding)
    )) + jObject
  }
}

object JLDSerializer {
  val base = "https://w3id.org/wm/cag/"

  val regroundingTypenames = Seq(
    JLDCorpus.typename,
    JLDExtraction.typename,
    JLDOntologyGroundings.typename,
    JLDOntologyGrounding.typename,
    JLDOntologyPredicateGrounding.typename
  )
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
    extends JLDObject(serializer, JLDOntologyGrounding.typename) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "ontologyConcept" -> name, // This should have escaped any slashes within a node name (simpleName)
    "value" -> value
  ))
}

object JLDOntologyGrounding {
  val typename = "Grounding"
  val singular = "grounding"
  val plural: String = singular // Mass noun
}

class JLDOntologyPredicateGrounding(serializer: JLDSerializer, predicateTuple: PredicateTuple, name: String, value: Float, display: String)
  extends JLDObject(serializer, JLDOntologyPredicateGrounding.typename) {

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "theme" -> new JLDOntologyGroundings(serializer, name, predicateTuple.theme).jldGroundings,
    "themeProperties" -> new JLDOntologyGroundings(serializer, name, predicateTuple.themeProperties).jldGroundings,
    "themeProcess" -> new JLDOntologyGroundings(serializer, name, predicateTuple.themeProcess).jldGroundings,
    "themeProcessProperties" -> new JLDOntologyGroundings(serializer, name, predicateTuple.themeProcessProperties).jldGroundings,
    "value" -> value,
    "display" -> display
  ))
}

object JLDOntologyPredicateGrounding {
  val typename = "PredicateGrounding"
  // These are unused because under Groundings the term "values" is used.
  val singular = "predicateGrounding"
  val plural: String = singular // Mass noun
}

class JLDOntologyGroundings(serializer: JLDSerializer, name: String, grounding: OntologyGrounding)
    extends JLDObject(serializer, JLDOntologyGroundings.typename) {
  val jldGroundings: Seq[JObject] = grounding.individualGroundings.map {
    case s: OntologyNodeGrounding =>
      new JLDOntologyGrounding(serializer, s.name, s.score).toJObject
    case pred: PredicateGrounding =>
      new JLDOntologyPredicateGrounding(serializer, pred.predicateTuple, name, pred.score, pred.name).toJObject
  }
//  val versionOpt = if (name == "wm") Some("a1a6dbee0296bdd2b81a4a751fce17c9ed0a3af8") else None

  override def toJObject: TidyJObject = TidyJObject(List(
    serializer.mkType(this),
    "name" -> name,
    "category" -> grounding.branchOpt,
    "version" -> grounding.versionOpt,
    "versionDate" -> grounding.dateOpt.map(_.toString),
    "values" -> jldGroundings
  ))
}

object JLDOntologyGroundings {
  val typename = "Groundings"
  val singular = "groundings"
  val plural: String = singular
}

class JLDModifier(serializer: JLDSerializer, quantifier: String, provenance: Option[Provenance],
    adjectiveGroundingOpt: Option[AdjectiveGrounding]) extends JLDObject(serializer, JLDModifier.typename) {
  val adjectiveGrounding: AdjectiveGrounding = adjectiveGroundingOpt.getOrElse(JLDModifier.noAdjectiveGrounding)

  override def toJObject: TidyJObject = {
    val jldProvenance = provenance.map(provenance => Seq(new JLDProvenance(serializer, provenance).toJObject))

    TidyJObject(List(
      serializer.mkType(this),
      "text" -> quantifier,
      JLDProvenance.singular -> jldProvenance,
      "intercept" -> adjectiveGrounding.intercept,
      "mu" -> adjectiveGrounding.mu,
      "sigma" -> adjectiveGrounding.sigma
    ))
  }
}

object JLDModifier {
  val singular = "modifier"
  val plural = "modifiers"
  val typename = "Modifier"

  val noAdjectiveGrounding: AdjectiveGrounding = AdjectiveGrounding(None, None, None)
}

abstract class JLDAttachment(serializer: JLDSerializer, kind: String)
    extends JLDObject(serializer, JLDAttachment.kind) {
}

object JLDAttachment {
  val singular = "state"
  val plural = "states"
  val kind = "State"
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
            val adjectiveGroundingOpt = triggeredAttachment.adjectiveGroundingsOpt.flatMap(_(index))

            new JLDModifier(serializer, quantifier, quantifierMention, adjectiveGroundingOpt).toJObject
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
@deprecated("This attachment is deprecated", "08-31-2020")
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

class JLDExtractionComparator(protected val jldExtraction: JLDExtraction) {
  lazy val provenance: Provenance = Provenance(jldExtraction.eidosMention.odinMention)
  lazy val provenances = jldExtraction.getMentions.map { eidosMention => Provenance(eidosMention.odinMention) }.sorted
  lazy val subtypeOrdering: Int = JLDCorpus.subtypeOrdering.indexOf(jldExtraction.subtypeString)
  lazy val isEventMention = subtypeOrdering == 1 || subtypeOrdering == 2
  lazy val triggerProvenanceOpt =
      if (isEventMention) Some(Provenance(jldExtraction.eidosMention.odinMention.asInstanceOf[EventMention].trigger))
      else None
  lazy val canonicalName: String = jldExtraction.eidosMention.canonicalName
  lazy val label: String = jldExtraction.eidosMention.odinMention.label
  lazy val attachmentsSize: Int = jldExtraction.eidosMention.odinMention.attachments.size
  lazy val attachments = jldExtraction.eidosMention.odinMention.attachments.toSeq
  lazy val attachmentNames = attachments.map(_.getClass.getName).sorted
  lazy val foundBy: String = jldExtraction.eidosMention.odinMention.foundBy
  lazy val attachmentPositions = attachments
      .map { attachment: Attachment =>
        attachment match {
          case triggeredAttachment: TriggeredAttachment =>
            triggeredAttachment.triggerProvenance.map(_.interval.start).getOrElse(-1)
          case _ => -2
        }
      }
      .sorted
  lazy val hash: Int = jldExtraction.eidosMention.odinMention.hashCode
}

object JLDExtractionComparator {
  val ordering = Unordered[JLDExtractionComparator]
      .orElseBy(_.provenance)
      .orElseBy(_.provenances)
      .orElseBy(_.subtypeOrdering)
      .orElseBy(_.triggerProvenanceOpt)
      .orElseBy(_.canonicalName)
      .orElseBy(_.label)
      .orElseBy(_.attachmentsSize)
      .orElseBy(_.attachmentNames)
      .orElseBy(_.foundBy)
      .orElseBy(_.attachmentPositions)
      .orElseBy(_.hash)
}

abstract class JLDExtraction(serializer: JLDSerializer, typeString: String, val subtypeString: String, val eidosMention: EidosMention)
    extends JLDObject(serializer, JLDExtraction.typename, eidosMention) with Regrounding {
  protected val extractionComparator = new JLDExtractionComparator(this)

  def getMentions: Seq[EidosMention] =  Seq.empty
  // This isn't necessary because attachments only show provenance, not reference to a different extraction
  //mention.eidosMentionsFromAttachments

  protected def provenance(): Seq[JValue] = Seq(new JLDProvenance(serializer, eidosMention).toJObject)

  // Copy these from Regrounding to avoid "inherits conflicting members".
  override def reToJObject: TidyJObject = toJObject(true)
  override def toJObject: TidyJObject = toJObject(false)

  override def toJObject(regrounding: Boolean): TidyJObject = {
    // Important: Since the attachments come from the odinMention and those mentions have not been
    // deduplicated like the eidosMentions have been, the attachments themselves can be duplicates.
    // However, in serialization of the attachments, like everything else, identity is used to make
    // the references.  This means that a duplicate, equals but not eq, count attribute will not be
    // found so that a reference cannot be generated and an exception will be thrown.  Perhaps
    // attachments need to be managed differently.
    val attachments = eidosMention.odinMention.attachments.toSeq
    val jldAttachments = attachments
        .collect{ case a: TriggeredAttachment => a }
        .sorted(TriggeredAttachment.alphaOrdering)
        .map(attachment => newJLDAttachment(attachment))
    val jldTimeAttachments = attachments
        .collect{ case a: Time => a }
        .sorted
        .map(attachment => newJLDAttachment(attachment))
    val jldLocationAttachments = attachments
        .collect{ case a: Location => a }
        .sorted
        .map(attachment => newJLDAttachment(attachment))
    val jldDctAttachments = attachments
        .collect{ case a: DCTime => a }
        .sorted
        .map(attachment => newJLDAttachment(attachment))

    // This might be used to test some groundings when they aren't configured to be produced.
    //val ontologyGroundings = mention.grounding.values.flatMap(_.grounding).toSeq
    //val ontologyGrounding = new OntologyGrounding(Seq(("hello", 4.5d), ("bye", 1.0d))).grounding
    val jldGroundings = {
      val groundings =
          if (eidosMention.grounding.isEmpty && eidosMention.deserializedGrounding.nonEmpty)
            eidosMention.deserializedGrounding
          else
            eidosMention.grounding
      val keys = groundings.keys.toSeq.sorted // for consistency

      keys.map { key =>
        val ontologyGroundings = groundings(key)

        new JLDOntologyGroundings(serializer, key, ontologyGroundings).toJObject
      }
    }
    val jldAllAttachments = (jldAttachments ++ jldTimeAttachments ++ jldLocationAttachments ++
        jldDctAttachments)
        .map(_.toJObject)

    if (regrounding)
      TidyJObject(List(
        serializer.mkType(this),
        serializer.mkId(this),
        "groundings" -> jldGroundings
      ))
    else
      TidyJObject(List(
        serializer.mkType(this),
        serializer.mkId(this),
        "type" -> typeString,
        "subtype" -> subtypeString,
        "labels" -> eidosMention.odinMention.labels,
        "text" -> eidosMention.odinMention.text,
        "rule" -> eidosMention.odinMention.foundBy,
        "canonicalName" -> eidosMention.canonicalName,
        "relevance" -> eidosMention.classificationOpt,
        "groundings" -> jldGroundings,
        JLDProvenance.singular -> provenance(),
        JLDAttachment.plural -> jldAllAttachments
      ))
  }
}

object JLDExtraction {
  implicit val ordering: Ordering[JLDExtraction] = JLDExtractionComparator.ordering.on(_.extractionComparator)

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

  def newJLDRelation(serializer: JLDSerializer, mention: EidosCrossSentenceEventMention): JLDRelation = {
    mention.odinMention.label match {
        // TODO: Figure out provenance for these!
//      case JLDRelationCorrelation.taxonomy => new JLDRelationCorrelation(serializer, mention, countAttachmentMap)
//      case JLDRelationCausation.taxonomy => new JLDRelationCausation(serializer, mention, countAttachmentMap)
      case _ => throw new IllegalArgumentException("Unknown CrossSentenceEventMention: " + mention)
    }
  }

  def newJLDRelation(serializer: JLDSerializer, mention: EidosEventMention): JLDRelation = {
    // This could be looked up in the taxonomy somehow, but the taxonomy doesn't include
    // information about the name of the arguments anyway, so may as well do all decoding here.
    // This could be pushed down to JLDRelation given EidosEventMention => JLDRelation.
    // If the JSON-LD specification doesn't change, then it is possible for the argument
    // names to be specified in master.yml file and then be taken over verbatim by querying the
    // arguments dictionary.
    mention.odinMention.label match {
      case JLDRelationCorrelation.taxonomy => new JLDRelationCorrelation(serializer, mention)
      case JLDRelationCausation.taxonomy => new JLDRelationCausation(serializer, mention)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + mention)
    }
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
//    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    super.getMentions ++ sources ++ targets /*++ triggers*/
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

class JLDRelationPositiveAffect(serializer: JLDSerializer, mention: EidosEventMention)
  extends JLDRelation(serializer, JLDRelationPositiveAffect.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationPositiveAffect.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationPositiveAffect.effect, Seq.empty).filter(isExtractable)
    //    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    super.getMentions ++ sources ++ targets /*++ triggers*/
  }

  override def toJObject: TidyJObject = {
    val trigger = new JLDTrigger(serializer, mention.eidosTrigger).toJObject
    val sources = mention.eidosArguments.getOrElse(JLDRelationPositiveAffect.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationPositiveAffect.effect, Seq.empty).filter(isExtractable)
    val jldArguments =
        sources.map(new JLDArgument(serializer, "source", _).toJObject) ++
        targets.map(new JLDArgument(serializer, "destination", _).toJObject)

    super.toJObject + TidyJObject(List(
      JLDTrigger.singular -> trigger,
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationPositiveAffect {
  val subtypeString = "positiveaffect"
  val taxonomy = "PositiveAffect"
  val cause = "cause"
  val effect = "effect"
}

class JLDRelationNegativeAffect(serializer: JLDSerializer, mention: EidosEventMention)
  extends JLDRelation(serializer, JLDRelationNegativeAffect.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationNegativeAffect.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationNegativeAffect.effect, Seq.empty).filter(isExtractable)
    //    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    super.getMentions ++ sources ++ targets /*++ triggers*/
  }

  override def toJObject: TidyJObject = {
    val trigger = new JLDTrigger(serializer, mention.eidosTrigger).toJObject
    val sources = mention.eidosArguments.getOrElse(JLDRelationNegativeAffect.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationNegativeAffect.effect, Seq.empty).filter(isExtractable)
    val jldArguments =
        sources.map(new JLDArgument(serializer, "source", _).toJObject) ++
        targets.map(new JLDArgument(serializer, "destination", _).toJObject)

    super.toJObject + TidyJObject(List(
      JLDTrigger.singular -> trigger,
      JLDArgument.plural -> jldArguments
    ))
  }
}

object JLDRelationNegativeAffect {
  val subtypeString = "negativeaffect"
  val taxonomy = "NegativeAffect"
  val cause = "cause"
  val effect = "effect"
}


class JLDRelationCorrelation(serializer: JLDSerializer, mention: EidosEventMention)
  extends JLDRelation(serializer, JLDRelationCorrelation.subtypeString, mention) {

  override def getMentions: Seq[EidosMention] = {
    val sources = mention.eidosArguments.getOrElse(JLDRelationCorrelation.cause, Seq.empty).filter(isExtractable)
    val targets = mention.eidosArguments.getOrElse(JLDRelationCorrelation.effect, Seq.empty).filter(isExtractable)
//    val triggers = Seq(mention.eidosTrigger) // Needed if extraction is to be read

    super.getMentions ++ sources ++ targets /*++ triggers*/
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
      super.getMentions ++ Seq(mention.eidosAnchor, mention.eidosNeighbor)

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

class JLDDependency(serializer: JLDSerializer, key: String, edge: (Int, Int, String), words: Seq[JLDWord])
    extends JLDObject(serializer, JLDDependency.typename) {

  override def toJObject: TidyJObject = {
    val source = words(edge._1).value
    val destination = words(edge._2).value
    val relation = edge._3

    TidyJObject(List(
      serializer.mkType(this),
      "type" -> key,
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
    val jldEdges = directedGraph.allEdges.map(new JLDDependency(serializer, key, _, words).toJObject)

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
    // This used to use the rawe text and not show the processed word.  However, that does not work well
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
    geoPhraseIDs: Seq[GeoPhraseID], relevanceOpt: Option[Float])
    extends JLDObject(serializer, JLDSentence.typename, sentence)
{
  protected def getSentenceText(sentence: Sentence): String = getSentenceFragmentText(sentence, 0, sentence.words.length)

  // This and the one above are copied almost verbatim from Sentence.scala.  We can't readily
  // use the version with the raw text because when the words are read back in, the conversion
  // is not performed again.
  protected def getSentenceFragmentText(sentence: Sentence, start: Int, end: Int): String = {
    if (end - start == 1)
      sentence.raw(start)
    else {
      val text = new mutable.StringBuilder()
      for (i <- start until end) {
        if (i > start) {
          // add as many white spaces as recorded between tokens
          val numberOfSpaces = math.max(1, sentence.startOffsets(i) - sentence.endOffsets(i - 1))

          0.until(numberOfSpaces).foreach { _ => text.append(" ") }
        }
        text.append(sentence.words(i)) // Changed from raw
      }
      text.toString()
    }
  }

  override def toJObject: TidyJObject = {
    val jldWords = sentence.words.indices.map(new JLDWord(serializer, document, sentence, _))
//    val sent_id = document.sentences.indexOf(sentence)
    val timexes: Seq[JObject] = timExs.map { timEx =>
      new JLDTimex(serializer, timEx).toJObject
    }
    val geoExps: Seq[JObject] = geoPhraseIDs.map { geoPhraseID =>
      new JLDGeoID(serializer, geoPhraseID).toJObject
    }
    // All graphs are now output.
    val keys = sentence.graphs.keys.toSeq.sorted
    // This is given access to the words because they are nicely in order and no searching need be done.
    val jldGraphMapPairs = keys.flatMap { key =>
      val dependencies = sentence.graphs(key)
      val arr = new JLDGraphMapPair(serializer, key, dependencies, jldWords).toJValue.asInstanceOf[JArray].arr

      arr
    }
    val rawTextOpt = document.text.map(_.substring(sentence.startOffsets.head, sentence.endOffsets.last))

    TidyJObject(List(
      serializer.mkType(this),
      serializer.mkId(this),
      "text" -> getSentenceText(sentence),
      "rawText" -> rawTextOpt,
      "relevance" -> relevanceOpt,
      JLDWord.plural -> jldWords.map(_.toJObject),
      JLDDependency.plural -> jldGraphMapPairs,
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
    val sentences = annotatedDocument.document.sentences
    val timExs = TimeNormFinder.getTimExs(annotatedDocument.odinMentions, sentences)
    val geoPhraseIDs = GeoNormFinder.getGeoPhraseIDs(annotatedDocument.odinMentions, sentences)
    val relevanceDocumentAttachmentOpt = RelevanceDocumentAttachment.getRelevanceDocumentAttachment(annotatedDocument.document)
    val jldSentences = sentences.zipWithIndex.map { case (sentence, index) =>
      new JLDSentence(serializer, annotatedDocument.document, sentence, timExs(index), geoPhraseIDs(index),
          relevanceDocumentAttachmentOpt.map(_.relevanceScores(index))).toJObject
    }.toSeq
    val jldText = annotatedDocument.document.text
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

class JLDCorpus protected (serializer: JLDSerializer, corpus: Corpus) extends JLDObject(serializer, JLDCorpus.typename, corpus)
    with Regrounding {

  def this(corpus: Corpus) = this(new JLDSerializer, corpus)

  def this(annotatedDocument: AnnotatedDocument) = this(Seq(annotatedDocument))

  protected def collectMentions(mentions: Seq[EidosMention], mapOfMentions: JIdentityHashMap[EidosMention, Int]):
      Seq[JLDExtraction] = {
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
    val mapOfMentions = new JIdentityHashMap[EidosMention, Int]()

    collectMentions(mentions, mapOfMentions)
  }


  case class SortRecord(document: Document, documentIndex: Int, jldExtraction: JLDExtraction, mention: Mention)

  protected def sortJldExtractions(jldExtractions: Seq[JLDExtraction], corpus: Corpus): Seq[JLDExtraction] = {
    val mapOfDocuments = {
      val mapOfDocuments = new JIdentityHashMap[Document, Int]()
      corpus.foreach { annotatedDocument =>
        mapOfDocuments.put(annotatedDocument.document, mapOfDocuments.size() + 1)
      }
      mapOfDocuments
    }
    val sortedJldExtractions = jldExtractions.sorted {
      Unordered[JLDExtraction]
        .orElseBy { extraction => mapOfDocuments.get(extraction.eidosMention.odinMention.document) }
        .orElseBy { extraction => extraction }
    }

    // Ensure that extractions are named/numbered nicely 1..n,
    // even though they have been discovered in a different order.
    serializer.reidentify(sortedJldExtractions)
    sortedJldExtractions
  }

  // Copy these from Regrounding to avoid "inherits conflicting members".
  override def reToJObject: TidyJObject = toJObject(true)
  override def toJObject: TidyJObject = toJObject(false)

  override def toJObject(regrounding: Boolean): TidyJObject = {
    // These are then on a per document basis.
    // TODO: Some of this seems to assume a single document, especially if attributes are compared by equals.
    val jldDocuments = corpus.map { annotatedDocument => new JLDDocument(serializer, annotatedDocument) }
    val jldDocumentObjects = jldDocuments.map(_.toJObject) // So that serializer has typenames
    val exposedEidosMentions = corpus.flatMap(_.eidosMentions)
    val allJldExtractions = collectMentions(exposedEidosMentions)
    val sortedJldExtractions = sortJldExtractions(allJldExtractions, corpus)
    val tidiedJldExtractions = sortedJldExtractions.map { jldExtraction =>
      if (regrounding) jldExtraction.toJObject(regrounding)
      else jldExtraction.toJObject
    }

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

    if (regrounding)
      TidyJObject(List(
        serializer.mkType(this),
        JLDExtraction.plural -> tidiedJldExtractions
      ))
    else
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

  val subtypeOrdering: Array[String] = Array(
    JLDConceptEntity.subtypeString,
    JLDRelationCausation.subtypeString,
    JLDRelationCorrelation.subtypeString,
    JLDRelationCoreference.subtypeString
  )
}
