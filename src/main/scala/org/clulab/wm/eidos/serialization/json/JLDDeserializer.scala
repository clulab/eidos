package org.clulab.wm.eidos.serialization.json

import java.time.LocalDateTime

import org.clulab.odin.Attachment
import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.SynPath
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Edge
import org.clulab.struct.GraphMap
import org.clulab.struct.Interval
import org.clulab.timenorm.formal.SimpleInterval
import org.clulab.timenorm.formal.UnknownInterval
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Hedging
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.attachments.{Property, Quantification}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.document.DCT
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.attachments.Provenance
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.TimeInterval
import org.clulab.wm.eidos.document.TimeStep
import org.clulab.wm.eidos.groundings.MultiOntologyGrounding
import org.clulab.wm.eidos.utils.Canonicalizer

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.annotation.tailrec
import scala.collection.Seq

class IdAndValue[ValueType](val id: String, val value: ValueType)

object IdAndValue {
  def toMap[ValueType](idsAndValues: Seq[IdAndValue[ValueType]]): Map[String, ValueType] =
      idsAndValues.map { idAndValue => idAndValue.id -> idAndValue.value }.toMap

  def toArray[ValueType](idsAndValues: Seq[IdAndValue[ValueType]]): Seq[ValueType] =
      idsAndValues.map(_.value) // .toArray
}

class IdAndDct(id: String, value: DCT) extends IdAndValue[DCT](id, value)

class IdAndTimex(id: String, value: TimeInterval) extends IdAndValue[TimeInterval](id, value)

class IdAndGeoPhraseId(id: String, value: GeoPhraseID) extends IdAndValue[GeoPhraseID](id, value)

case class WordSpec(startOffset: Int, endOffset: Int, word: String, tag: String, lemma: String, entity: String,
    norm: String, chunk: String)
class IdAndWordSpec(id: String, value: WordSpec) extends IdAndValue[WordSpec](id, value)

class IdAndSentence(id: String, value: Sentence) extends IdAndValue[Sentence](id, value)

case class SentencesSpec(sentences: Array[Sentence], sentenceMap: Map[String, Int],
    timexes: Array[Seq[TimeInterval]], timexMap: Map[String, TimeInterval],
    geolocs: Array[Seq[GeoPhraseID]], geolocMap: Map[String, GeoPhraseID])

class IdAndDocument(id: String, value: EidosDocument) extends IdAndValue(id, value)

case class DocumentSpec(idAndDocument: IdAndDocument, sentencesSpec: SentencesSpec)

class IdAndMention(id: String, value: Mention) extends IdAndValue[Mention](id,value)

case class Extraction(id: String, extractionType: String, extractionSubtype: String, provenance: Provenance,
    triggerProvenanceOpt: Option[Provenance], argumentMap: Map[String, Seq[String]])

object JLDDeserializer {
  type DocumentMap = Map[String, EidosDocument]
  type SentenceMap = Map[String, Int]
  // So do documentSentenceMap(documentId)(sentenceId) to get the Int
  type DocumentSentenceMap = Map[String, SentenceMap]
  type MentionMap = Map[String, Mention]
  type GeolocMap = Map[String, GeoPhraseID]
  type TimexMap = Map[String, TimeInterval]

  type ProvenanceMap = Map[Provenance, String] // Do we really want this document involved?
}

class JLDDeserializer {
  import org.clulab.wm.eidos.serialization.json.JLDDeserializer._
  implicit val formats = org.json4s.DefaultFormats

  protected def requireType(jValue: JValue, typeName: String): Unit =
      require((jValue \ "@type").extract[String] == typeName)

  protected def id(jValue: JValue): String =
      (jValue \ "@id").extract[String]

  protected def toInterval(start: Int, end: Int, offset: Int, inclusiveEnd: Boolean): Interval = {
    // Coordinate this with JLDSerializer.
    val endCorrection = if (inclusiveEnd) -1 else 0
    Interval(start - offset, end - offset - endCorrection)
  }

  protected def nothingToNone(jValueOpt: Option[JValue]): Option[JValue] = {
    jValueOpt match {
      case Some(JNothing) => None
      case None => None
      case value @ Some(_) => value
    }
  }

  def deserializeDct(dctValueOpt: Option[JValue]): Option[IdAndDct] = {
    dctValueOpt.map { dctValue =>
      requireType(dctValue, JLDDCT.typename)
      val dctId = id(dctValue)
      val text = (dctValue \ "text").extract[String]
      val optStart = (dctValue \ "start").extractOpt[String]
      val optEnd = (dctValue \ "end").extractOpt[String]

      val dct =
        if (optStart.isEmpty && optEnd.isEmpty) DCT(UnknownInterval, text)
        else {
          val start = optStart.getOrElse(optEnd.get)
          val end = optEnd.getOrElse(optStart.get)
          val startDateTime = LocalDateTime.parse(start)
          val endDateTime = LocalDateTime.parse(end)
          val interval = SimpleInterval(startDateTime, endDateTime)

          DCT(interval, text)
        }

      new IdAndDct(dctId, dct)
    }
  }

  def deserializeTimeInterval(timeIntervalValue: JValue): TimeStep = {
    requireType(timeIntervalValue, JLDTimeInterval.typename)
    val timeIntervalId = id(timeIntervalValue) // This is never used, so why do we have it?
    val start = LocalDateTime.parse((timeIntervalValue \ "start").extract[String])
    val end = LocalDateTime.parse((timeIntervalValue \ "end").extract[String])
    val duration = (timeIntervalValue \ "duration").extract[Int]

    TimeStep(start, end, duration)
  }

  // TODO function name and variable names match text names in file
  // return type matches type names (left column)
  def deserializeTimex(timexValue: JValue): IdAndTimex = {
    requireType(timexValue, JLDTimex.typename)
    val timexId = id(timexValue)
    val text = (timexValue \ "text").extract[String]
    val startOffset = (timexValue \ "startOffset").extract[Int]
    val endOffset = (timexValue \ "endOffset").extract[Int]
    val intervals = (timexValue \ "intervals").extract[JArray].arr.map { timeIntervalValue =>
      deserializeTimeInterval(timeIntervalValue)
    }
    new IdAndTimex(timexId, TimeInterval(Interval(startOffset, endOffset), intervals, text))
  }

  def deserializeGeoloc(geoIdValue: JValue): IdAndGeoPhraseId = {
    requireType(geoIdValue, JLDGeoID.typename)
    val geoIdId = id(geoIdValue)
    val startOffset = (geoIdValue \ "startOffset").extract[Int]
    val endOffset = (geoIdValue \ "endOffset").extract[Int]
    val text = (geoIdValue \ "text").extract[String]
    val geoId = (geoIdValue \ "geoID").extractOpt[String].map(Integer.parseInt)
    val geoPhraseId = new GeoPhraseID(text, geoId, startOffset, endOffset)

    new IdAndGeoPhraseId(geoIdId, geoPhraseId)
  }

  def deserializeWordData(wordDataValue: JValue): IdAndWordSpec = {
    requireType(wordDataValue, JLDWord.typename)
    val wordId = id(wordDataValue)
    val text = (wordDataValue \ "text").extract[String]
    val tag = (wordDataValue \ "tag").extract[String]
    val entity = (wordDataValue \ "entity").extract[String]
    val startOffset = (wordDataValue \ "startOffset").extract[Int]
    val endOffset = (wordDataValue \ "endOffset").extract[Int]
    val lemma = (wordDataValue \ "lemma").extract[String]
    val chunk = (wordDataValue \ "chunk").extract[String]
    val norm = (wordDataValue \ "norm").extract[String]
    val wordData = WordSpec(startOffset, endOffset, text, tag, lemma, entity, norm, chunk)

    new IdAndWordSpec(wordId, wordData)
  }

  def deserializeDependency(dependencyValue: JValue, wordMap: Map[String, Int]): Edge[String] = {
    requireType(dependencyValue, JLDDependency.typename)
    val source = id(dependencyValue \ "source")
    val destination = id(dependencyValue \ "destination")
    val relation = (dependencyValue \ "relation").extract[String]
    val edge: Edge[String] = Edge(wordMap(source), wordMap(destination), relation)

    edge
  }

  def deserializeSentences(sentencesValue: JValue): SentencesSpec = {
    // Keep global map because references can be used outside of this sentence context.
    var timexes: List[Seq[TimeInterval]] = List.empty
    var timexMap: Map[String, TimeInterval] = Map.empty
    var geolocs: List[Seq[GeoPhraseID]] = List.empty
    var geolocMap: Map[String, GeoPhraseID] = Map.empty

    val idsAndSentences = sentencesValue.extract[JArray].arr.map { sentenceValue: JValue =>
      requireType(sentenceValue, JLDSentence.typename)
      val sentenceId = id(sentenceValue)
      val raw: Array[String] = (sentenceValue \ "text").extract[String].split(' ')

      val idsAndWordSpecs = (sentenceValue \ "words").extract[JArray].arr.map(deserializeWordData).toArray
      val wordMap = idsAndWordSpecs.indices.map(index => idsAndWordSpecs(index).id -> index).toMap // why not directly to wordspec?

      val dependencies = (sentenceValue \ "dependencies").extract[JArray].arr.map { dependencyValue: JValue =>
        deserializeDependency(dependencyValue, wordMap)
      }
      val sourceRoots = dependencies.map(_.source).toSet
      val destinationRoots = dependencies.map(_.destination).toSet
      val roots = sourceRoots ++ destinationRoots
      val graph = DirectedGraph[String](dependencies, roots)

      val idsAndTimexes = (sentenceValue \ "timexes").extractOpt[JArray].map { jArray =>
        jArray.arr.map(deserializeTimex)
      }.getOrElse(List.empty)
      timexes = timexes :+ idsAndTimexes.map(_.value)
      timexMap = timexMap ++ idsAndTimexes.map { idAndTimex => idAndTimex.id -> idAndTimex.value }

      val idsAndGeolocs = (sentenceValue \ "geolocs").extractOpt[JArray].map { jArray =>
        jArray.arr.map(deserializeGeoloc)
      }.getOrElse(List.empty)
      geolocs = geolocs :+ idsAndGeolocs.map(_.value)
      geolocMap = geolocMap ++ idsAndGeolocs.map { idAndGeoloc => idAndGeoloc.id -> idAndGeoloc.value }

      val startOffsets: Array[Int] = idsAndWordSpecs.map(it => it.value.startOffset)
      val endOffsets: Array[Int] = idsAndWordSpecs.map(it => it.value.endOffset)
      val words: Array[String] = idsAndWordSpecs.map(it => it.value.word)
      val sentence = Sentence(raw, startOffsets, endOffsets, words)
      sentence.tags = Some(idsAndWordSpecs.map(it => it.value.tag))
      sentence.lemmas = Some(idsAndWordSpecs.map(it => it.value.lemma))
      sentence.entities = Some(idsAndWordSpecs.map(it => it.value.entity))
      sentence.norms = Some(idsAndWordSpecs.map(it => it.value.norm))
      sentence.chunks = Some(idsAndWordSpecs.map(it => it.value.chunk))
      sentence.syntacticTree = None // Documented
      sentence.graphs = GraphMap(Map(GraphMap.UNIVERSAL_ENHANCED -> graph))
      sentence.relations = None // Documented
      new IdAndSentence(sentenceId, sentence)
    }

    val sentences = idsAndSentences.map(_.value).toArray // Document needs an Array[Sentence]
    // This is because Mention only uses index of sentence in document, not reference to Sentence itself.
    val sentenceMap = idsAndSentences.indices.map { index => idsAndSentences(index).id -> index }.toMap

    SentencesSpec(sentences, sentenceMap, timexes.toArray, timexMap, geolocs.toArray, geolocMap)
  }

  def deserializeInterval(intervalValue: JValue, offset: Int, inclusiveEnd: Boolean): Interval = {
    requireType(intervalValue, JLDInterval.typename)
    val start = (intervalValue \ "start").extract[Int]
    val end = (intervalValue \ "end").extract[Int]
    val interval = toInterval(start, end, offset, inclusiveEnd)

    interval
  }

  protected def deserializeSingularProvenance(provenanceValue: JValue, documentMap: DocumentMap,
      documentSentenceMap: DocumentSentenceMap): Provenance = {
    requireType(provenanceValue, JLDProvenance.typename)
    val documentId = id(provenanceValue \ "document")
    val document = documentMap(documentId)

    val sentenceId = id(provenanceValue \ "sentence")
    val sentence = documentSentenceMap(documentId)(sentenceId)

    val sentenceWordPositions = (provenanceValue \ "sentenceWordPositions").extract[JArray].arr.map { intervalValue =>
      deserializeInterval(intervalValue, offset = 1, inclusiveEnd = true)
    }
    require(sentenceWordPositions.size == 1)

    Provenance(document, sentence, sentenceWordPositions.head)
  }

  protected def deserializePluralProvenance(provenanceValue: JValue, documentMap: DocumentMap,
      documentSentenceMap: DocumentSentenceMap): Provenance = {
    val provenanceValues: JArray = provenanceValue.asInstanceOf[JArray]
    require(provenanceValues.arr.size == 1)

    deserializeSingularProvenance(provenanceValues.arr.head, documentMap, documentSentenceMap)
  }

  def deserializeProvenance(provenanceValueOpt: Option[JValue], documentMap: DocumentMap,
      documentSentenceMap: DocumentSentenceMap): Option[Provenance] =
      provenanceValueOpt.map(deserializePluralProvenance(_, documentMap, documentSentenceMap))

  def deserializeDocument(documentValue: JValue): DocumentSpec = {
    requireType(documentValue, JLDDocument.typename)
    val documentId = id(documentValue)
    val title = (documentValue \ "title").extractOpt[String]
    val text = (documentValue \ "text").extractOpt[String]
    val idAndDctOpt = deserializeDct(nothingToNone((documentValue \ "dct").extractOpt[JValue]))
    val sentencesSpec = deserializeSentences(documentValue \ "sentences")
    val timexCount = sentencesSpec.timexes.map(_.size).sum
    val geolocsCount = sentencesSpec.geolocs.map(_.size).sum
    val sentences = sentencesSpec.sentences
    val eidosDocument = new EidosDocument(sentences, text)

    eidosDocument.id = title
    eidosDocument.text = text
    // We really don't know whether time and geo were on or not.  If none were found
    // at all, then assume they were off and use None rather than Some(List.empty).
    eidosDocument.times = if (timexCount == 0) None else Some(sentencesSpec.timexes)
    eidosDocument.geolocs = if (geolocsCount == 0) None else Some(sentencesSpec.geolocs)
    eidosDocument.dct = idAndDctOpt.map(_.value)

    val idAndDocument = new IdAndDocument(documentId, eidosDocument)
    DocumentSpec(idAndDocument, sentencesSpec)
  }

  def deserializeTrigger(triggerValueOpt: Option[JValue], documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap): Option[Provenance] = {
    nothingToNone(triggerValueOpt).map { triggerValue =>
      requireType(triggerValue, JLDTrigger.typename)
      val provenance = deserializeProvenance((triggerValue \ "provenance").extractOpt[JValue], documentMap, documentSentenceMap).get

      provenance
    }
  }

  def deserializeArguments(argumentsValueOpt: Option[JValue]): Map[String, Seq[String]] = {
    var argumentMap: Map[String, Seq[String]] = Map.empty

    nothingToNone(argumentsValueOpt).foreach { argumentsValue =>
      val argumentsValues: JArray = argumentsValue.asInstanceOf[JArray]
      val arguments = argumentsValues.arr.map { argumentsValue =>
        requireType(argumentsValue, JLDArgument.typename)
        val name = (argumentsValue \ "type").extract[String]
        val argumentId = id(argumentsValue \ "value")

        if (argumentMap.contains(name)) {
          val oldValues = argumentMap(name)
          // It is done this way to keep the values in order
          val newValues = oldValues :+ argumentId

          argumentMap += (name -> newValues)
        }
        else
          argumentMap += (name -> List(argumentId))
      }
    }
    argumentMap
  }

  def deserializeExtraction(extractionValue: JValue, documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap): Extraction = {
    requireType(extractionValue, JLDExtraction.typename)
    val extractionId = id(extractionValue)
    val extractionType = (extractionValue \ "type").extract[String]
    val extractionSubtype = (extractionValue \ "subtype").extract[String]
    val provenance = deserializeProvenance((extractionValue \ "provenance").extractOpt[JValue], documentMap, documentSentenceMap).get
    val triggerProvenanceOpt = deserializeTrigger((extractionValue \ "trigger").extractOpt[JValue], documentMap, documentSentenceMap)
    val argumentMap = deserializeArguments((extractionValue \ "arguments").extractOpt[JValue])

    Extraction(extractionId, extractionType, extractionSubtype, provenance, triggerProvenanceOpt, argumentMap)
  }

  protected def deserializeModifier(modifierValue: JValue, documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap): (String, Provenance) = {
    requireType(modifierValue, JLDModifier.typename)
    val text = (modifierValue \ "text").extract[String]
    val provenance = deserializeProvenance((modifierValue \ "provenance").extractOpt[JArray],
        documentMap, documentSentenceMap).get

    (text, provenance)
  }

  def deserializeModifiers(modifiersValueOpt: Option[JArray], documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap):
      (Option[Seq[String]], Option[Seq[Provenance]]) = {
    if (modifiersValueOpt.isDefined) {
      val textsAndProvenances = modifiersValueOpt.get.arr.map { modifierValue =>
        deserializeModifier(modifierValue, documentMap, documentSentenceMap)
      }
      val texts = textsAndProvenances.map(_._1)
      val provenances = textsAndProvenances.map(_._2)

      (Option(texts), Option(provenances))
    }
    else
      (None, None)
  }

  def deserializeState(stateValue: JValue, documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap,
      timexMap: TimexMap, geolocMap: GeolocMap): Attachment = {
    requireType(stateValue, JLDAttachment.kind)
    val stateType = (stateValue \ "type").extract[String]
    val text = (stateValue \ "text").extract[String]
    val provenanceOpt = deserializeProvenance((stateValue \ "provenance").extractOpt[JArray],
        documentMap, documentSentenceMap)
    val modifiersValueOpt = (stateValue \ "modifiers").extractOpt[JArray]
    val (quantifiers, quantifierProvenances) = deserializeModifiers(modifiersValueOpt, documentMap, documentSentenceMap)
    val attachment = stateType match {
      case "QUANT" =>
        require(provenanceOpt.isDefined)
        new Quantification(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "INC" =>
        require(provenanceOpt.isDefined)
        new Increase(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "DEC" =>
        require(provenanceOpt.isDefined)
        new Decrease(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "PROP" =>
        require(provenanceOpt.isEmpty)
        new Property(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "HEDGE" =>
        // require(provenanceOpt.isDefined) // todo add this back
        new Hedging(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "NEGATION" =>
        // require(provenanceOpt.isDefined) // todo add this back
        new Negation(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "TIMEX" =>
        require(provenanceOpt.isEmpty)
        val value = id((stateValue \ "value").extract[JValue])
        val timeInterval = timexMap(value)

        new Time(timeInterval)
      case "LocationExp" =>
        require(provenanceOpt.isEmpty)
        val value = id((stateValue \ "value").extract[JValue])
        val geoPhraseId = geolocMap(value)

        new Location(geoPhraseId)
      case _ =>
        throw new Exception(s"Unknown state type $stateType")
    }

    attachment
  }

  def deserializeStates(statesValueOpt: Option[JArray], documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap,
      timexMap: TimexMap, geolocMap: GeolocMap): Set[Attachment] = {
    val attachments = statesValueOpt.map { statesValue =>
      statesValue.arr.map { stateValue =>
        deserializeState(stateValue, documentMap, documentSentenceMap, timexMap, geolocMap)
      }.toSet
    }.getOrElse(Set.empty)

    attachments
  }

  def deserializeMention(extractionValue: JValue, extraction: Extraction, mentionMap: Map[String, Mention],
      documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap, timexMap: TimexMap,
      geolocMap: GeolocMap, provenanceMap: ProvenanceMap): Mention = {
    requireType(extractionValue, JLDExtraction.typename)
    val extractionId = extraction.id
    val extractionType = extraction.extractionType
    val extractionSubtype = extraction.extractionSubtype
    val labels = (extractionValue \ "labels").extract[List[String]]
    val tokenInterval = extraction.provenance.interval
    val triggerOpt: Option[TextBoundMention] = extraction.triggerProvenanceOpt.map { provenance =>
      mentionMap(provenanceMap(provenance)).asInstanceOf[TextBoundMention]
    }
    val sentence = extraction.provenance.sentence
    val document = extraction.provenance.document
    val keep = true // Documented
    val foundBy = (extractionValue \ "rule").extract[String]
    val paths: Map[String, Map[Mention, SynPath]] = Map.empty // Documented
    val misnamedArguments: Map[String, Seq[Mention]] = extraction.argumentMap.map { case (name, ids) =>
      (name -> ids.map { id => mentionMap(id) })
    }
    val attachments: Set[Attachment] = deserializeStates((extractionValue \ "states").extractOpt[JArray],
        documentMap, documentSentenceMap, timexMap, geolocMap)
    val mention =
        if (extractionType == "concept" && extractionSubtype == "entity") {
          require(triggerOpt.isEmpty)
          require(misnamedArguments.isEmpty)
          new TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments)
        }
        else if (extractionType == "relation" && extractionSubtype == "causation") {
          require(JLDRelationCausation.taxonomy == labels.head)
          require(triggerOpt.isDefined)
          require(misnamedArguments.get("source").nonEmpty)
          require(misnamedArguments.get("destination").nonEmpty)
          val renamedArguments: Map[String, Seq[Mention]] = Map(
            ("cause" -> misnamedArguments("source")),
            ("effect" -> misnamedArguments("destination"))
          )
          new EventMention(labels, tokenInterval, triggerOpt.get, renamedArguments, paths, sentence, document,
              keep, foundBy, attachments)
        }
        else if (extractionType == "relation" && extractionSubtype == "correlation") {
          require(JLDRelationCorrelation.taxonomy == labels.head)
          require(triggerOpt.isDefined)
          require(misnamedArguments.get("argument").nonEmpty)
          require(misnamedArguments("argument").size == 2)
          val renamedArguments: Map[String, Seq[Mention]] = Map(
            ("cause" -> Seq(misnamedArguments("argument")(0))),
            ("effect" -> Seq(misnamedArguments("argument")(1)))
          )
          new EventMention(labels, tokenInterval, triggerOpt.get, renamedArguments, paths, sentence, document,
              keep, foundBy, attachments)
        }
        else if (extractionType == "relation" && extractionSubtype == "coreference") {
          require(JLDRelationCoreference.taxonomy == labels.head)
          require(triggerOpt.isEmpty)
          require(misnamedArguments.get("anchor").nonEmpty)
          require(misnamedArguments.get("reference").nonEmpty)
          require(misnamedArguments.get("anchor").size == 1)
          require(misnamedArguments.get("reference").size == 1)
          val anchor = misnamedArguments("anchor")(0)
          val neighbor = misnamedArguments("reference")(0)
          // These arguments seem to get duplicated from the anchor and neighbor.
          // See org.clulab.odin.impl.CrossSentenceExtractor.mkMention.
          val renamedArguments: Map[String, Seq[Mention]] = Map(
            ("cause" -> misnamedArguments("anchor")),
            ("effect" -> misnamedArguments("reference"))
          )
          new CrossSentenceMention(labels, anchor, neighbor, renamedArguments, document,
              keep, foundBy, attachments)
        }
        else
          throw new Exception(s"Unknown extraction type = $extractionType, subtype = $extractionSubtype")

    mention
  }

  @tailrec
  final def deserializeMentions(extractionsValue: JArray, extractions: Seq[Extraction],
      mentionMap: Map[String, Mention], provenanceMap: Map[Provenance, String],
      documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap,
      timexMap: TimexMap, geolocMap: GeolocMap): Map[String, Mention] = {

    def isRipe(extraction: Extraction): Boolean = {
      val hasAllArguments = extraction.argumentMap.forall { case (key, mentionIds) =>
        mentionIds.forall { mentionId =>
          (mentionMap.contains(mentionId))
        }
      }
      val hasTrigger = extraction.triggerProvenanceOpt.forall(provenanceMap.contains)

      hasAllArguments && hasTrigger
    }

    if (extractions.isEmpty)
      mentionMap
    else {
      val groupedExtractions = extractions.groupBy(isRipe)
      val ripeExtractions = groupedExtractions.getOrElse(true, Seq.empty)
      require(ripeExtractions.nonEmpty)
      val unripeExtractions = groupedExtractions.getOrElse(false, Seq.empty)
      val idsAndMentions = ripeExtractions.map { extraction =>
        val extractionValue = extractionsValue.arr.find { extractionValue =>
          requireType(extractionValue, JLDExtraction.typename)
          val extractionId = id(extractionValue)

          extractionId == extraction.id
        }.get
        val mention = deserializeMention(extractionValue, extraction, mentionMap, documentMap, documentSentenceMap,
            timexMap, geolocMap, provenanceMap)

        new IdAndMention(extraction.id, mention)
      }
      val newMentionMap = mentionMap ++ IdAndValue.toMap[Mention](idsAndMentions)
      val newProvenanceMap = provenanceMap ++ ripeExtractions.zip(idsAndMentions).map { case (extraction, idAndMention) =>
        extraction.provenance -> idAndMention.id
      }

      deserializeMentions(extractionsValue, unripeExtractions, newMentionMap, newProvenanceMap,
          documentMap, documentSentenceMap, timexMap, geolocMap)
    }
  }

  protected def removeTriggerOnlyMentions(mentions: Seq[Mention]): Seq[Mention] = {
    var argumentMentions = mentions.flatMap { mention => mention.arguments.values.flatten }
    var triggerMentions = mentions.collect {
      case eventMention: EventMention => eventMention.trigger
    }
    var triggerOnlyMentions = triggerMentions.filter { triggerMention =>
      !argumentMentions.exists { argumentMention =>
        triggerMention.eq(argumentMention) // eq is important here
      }
    }
    var remainingMentions = mentions.filter { mention =>
      !triggerOnlyMentions.exists { triggerOnlyMention =>
        mention.eq(triggerOnlyMention)
      }
    }

    remainingMentions
  }

  def deserializeCorpus(corpusValue: JValue, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounding): Corpus = {
    requireType(corpusValue, JLDCorpus.typename)
    // Check to see about DCT value and if it has id
    val documentSpecs = (corpusValue \ "documents").extract[JArray].arr.map(deserializeDocument)
    val documentMap: DocumentMap = documentSpecs.map { documentSpec =>
      documentSpec.idAndDocument.id -> documentSpec.idAndDocument.value
    }.toMap
    val documentSentenceMap: DocumentSentenceMap = documentSpecs.map { documentSpec =>
      documentSpec.idAndDocument.id -> documentSpec.sentencesSpec.sentenceMap
    }.toMap
    // Combine all the timexes and geolocs of all corpora
    val timexMap = documentSpecs.flatMap { documentSpec =>
      documentSpec.sentencesSpec.timexMap
    }.toMap
    val geolocMap = documentSpecs.flatMap { documentSpec =>
      documentSpec.sentencesSpec.geolocMap
    }.toMap
    val extractionsValueOpt = (corpusValue \ "extractions").extractOpt[JArray]
    val extractions = extractionsValueOpt.map { extractionsValue =>
      extractionsValue.arr.map { extractionValue =>
        deserializeExtraction(extractionValue, documentMap, documentSentenceMap)
      }
    }.getOrElse(Seq.empty[Extraction])
    val mentionMap = extractionsValueOpt.map { extractionsValue =>
      deserializeMentions(extractionsValue, extractions, Map.empty, Map.empty,
        documentMap, documentSentenceMap, timexMap, geolocMap)
    }.getOrElse(Map.empty)
    val allOdinMentions = mentionMap.values.toArray
    val odinMentions = removeTriggerOnlyMentions(allOdinMentions)
    val eidosMentions = EidosMention.asEidosMentions(odinMentions, canonicalizer, ontologyGrounder)
    val annotatedDocuments = documentSpecs.map { documentSpec =>
      AnnotatedDocument(documentSpec.idAndDocument.value, odinMentions, eidosMentions)
    }
    val corpus = annotatedDocuments

    corpus
  }

  def deserialize(json: String, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounding): Corpus = {
    val jValue: JValue = parse(json)
    val corpus = deserializeCorpus(jValue, canonicalizer, ontologyGrounder)
    corpus
  }
}
