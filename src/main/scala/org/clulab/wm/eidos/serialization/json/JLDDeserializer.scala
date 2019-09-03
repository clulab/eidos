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
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.attachments.DCTime
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Hedging
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.attachments.{Property, Quantification}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.attachments.Provenance
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.document.DctDocumentAttachment
import org.clulab.wm.eidos.groundings.MultiOntologyGrounding
import org.clulab.wm.eidos.utils.Canonicalizer
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

class IdAndTimex(id: String, value: TimEx) extends IdAndValue[TimEx](id, value)

class IdAndGeoPhraseId(id: String, value: GeoPhraseID) extends IdAndValue[GeoPhraseID](id, value)

case class WordSpec(startOffset: Int, endOffset: Int, word: String, tag: String, lemma: String, entity: String,
    norm: String, chunk: String)
class IdAndWordSpec(id: String, value: WordSpec) extends IdAndValue[WordSpec](id, value)

class IdAndSentence(id: String, value: Sentence) extends IdAndValue[Sentence](id, value)

case class SentencesSpec(sentences: Array[Sentence], sentenceMap: Map[String, Int],
    timexes: Array[Seq[TimEx]], timexMap: Map[String, TimEx],
    geolocs: Array[Seq[GeoPhraseID]], geolocMap: Map[String, GeoPhraseID])

class IdAndDocument(id: String, value: Document) extends IdAndValue(id, value)

case class DocumentSpec(idAndDocument: IdAndDocument, idAndDctOpt: Option[IdAndDct], sentencesSpec: SentencesSpec)

class IdAndMention(id: String, value: Mention) extends IdAndValue[Mention](id,value)

case class Extraction(id: String, extractionType: String, extractionSubtype: String, provenance: Provenance,
    triggerProvenanceOpt: Option[Provenance], argumentMap: Map[String, Seq[String]])

object JLDDeserializer {
  type DocumentMap = Map[String, Document]
  type SentenceMap = Map[String, Int]
  // So do documentSentenceMap(documentId)(sentenceId) to get the Int
  type DocumentSentenceMap = Map[String, SentenceMap]
  type MentionMap = Map[String, Mention]
  type GeolocMap = Map[String, GeoPhraseID]
  type TimexMap = Map[String, TimEx]
  type DctMap = Map[String, DCT]

  type ProvenanceMap = Map[Provenance, String] // Do we really want this document involved?
}

class JLDDeserializer {
  import org.clulab.wm.eidos.serialization.json.JLDDeserializer._
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

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
      val start = (dctValue \ "start").extract[String]
      val end = (dctValue \ "end").extract[String]

      val dct = {
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
    val _ = id(timeIntervalValue) // This is never used, so why do we have it?
    val start = (timeIntervalValue \ "start").extract[String]
    val end = (timeIntervalValue \ "end").extract[String]
    val startDate = LocalDateTime.parse(start)
    val endDate = LocalDateTime.parse(end)

    TimeStep(startDate, endDate)
  }

  def deserializeTimex(timexValue: JValue): IdAndTimex = {
    requireType(timexValue, JLDTimex.typename)
    val timexId = id(timexValue)
    val text = (timexValue \ "text").extract[String]
    val startOffset = (timexValue \ "startOffset").extract[Int]
    val endOffset = (timexValue \ "endOffset").extract[Int]
    val intervals = (timexValue \ "intervals").extractOpt[JArray] match {
      case Some(timeIntervalsValue: JArray) => timeIntervalsValue.arr.map { timeIntervalValue: JValue =>
        deserializeTimeInterval(timeIntervalValue)
      }
      case _ => Seq()
    }
    new IdAndTimex(timexId, TimEx(Interval(startOffset, endOffset), intervals, text))
  }

  def deserializeGeoloc(geoIdValue: JValue): IdAndGeoPhraseId = {
    requireType(geoIdValue, JLDGeoID.typename)
    val geoIdId = id(geoIdValue)
    val startOffset = (geoIdValue \ "startOffset").extract[Int]
    val endOffset = (geoIdValue \ "endOffset").extract[Int]
    val text = (geoIdValue \ "text").extract[String]
    val geoId = (geoIdValue \ "geoID").extractOpt[String].map(Integer.parseInt)
    val geoPhraseId = GeoPhraseID(text, geoId, startOffset, endOffset)

    new IdAndGeoPhraseId(geoIdId, geoPhraseId)
  }

  def deserializeWordData(wordDataValue: JValue): IdAndWordSpec = {
    requireType(wordDataValue, JLDWord.typename)
    val wordId = id(wordDataValue)
    // extractOpt is necessary because now the processed word is being stored rather than
    // the raw one.  The processed version can be empty in which case it will be tidied on
    // serialization and not be available here.
    val text = (wordDataValue \ "text").extractOpt[String].getOrElse("")
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

  protected def mkRaw(idsAndWordSpecs: Array[IdAndWordSpec], documentText: Option[String]): Array[String] = {
    idsAndWordSpecs.map { idAndWordSpec =>
      val wordSpec = idAndWordSpec.value
      val raw = documentText.get.substring(wordSpec.startOffset, wordSpec.endOffset)

      raw
    }
  }

  def deserializeSentences(sentencesValue: JValue, documentText: Option[String]): SentencesSpec = {
    // Keep global map because references can be used outside of this sentence context.
    var timexes: List[Seq[TimEx]] = List.empty
    var timexMap: Map[String, TimEx] = Map.empty
    var geolocs: List[Seq[GeoPhraseID]] = List.empty
    var geolocMap: Map[String, GeoPhraseID] = Map.empty
    var sentencesOpt = sentencesValue.extractOpt[JArray].map(_.arr)
    val idsAndSentences = sentencesOpt.map { sentences => sentences.map { sentenceValue: JValue =>
      requireType(sentenceValue, JLDSentence.typename)
      val sentenceId = id(sentenceValue)
      // A sentence, if it exists at all, must have words; therefore, no extractOpt here.
      val idsAndWordSpecs = (sentenceValue \ "words").extract[JArray].arr.map(deserializeWordData).toArray
      val wordMap = idsAndWordSpecs.indices.map(index => idsAndWordSpecs(index).id -> index).toMap // why not directly to wordspec?
      // This doesn't work if there are double spaces in the text.  Too many elements will be made.
      // val raw: Array[String] = (sentenceValue \ "text").extract[String].split(' ')
      val graphMap = (sentenceValue \ "dependencies").extractOpt[JArray].map { dependenciesValue =>
        val dependencies = dependenciesValue.arr.map { dependencyValue: JValue =>
          deserializeDependency(dependencyValue, wordMap)
        }
        val sourceRoots = dependencies.map(_.source).toSet
        val destinationRoots = dependencies.map(_.destination).toSet
        val roots = sourceRoots ++ destinationRoots
        val graph = DirectedGraph[String](dependencies, roots)

        GraphMap(Map(GraphMap.UNIVERSAL_ENHANCED -> graph))
      }.getOrElse(new GraphMap)

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

      // IntelliJ doesn't like these, but the compiler is OK with them.
      val startOffsets: Array[Int] = idsAndWordSpecs.map(idAndSpec => idAndSpec.value.startOffset)
      val endOffsets: Array[Int] = idsAndWordSpecs.map(idAndSpec => idAndSpec.value.endOffset)
      val raw = mkRaw(idsAndWordSpecs, documentText)
      val words: Array[String] = idsAndWordSpecs.map(idAndSpec => idAndSpec.value.word)
      val sentence = Sentence(raw, startOffsets, endOffsets, words)
      sentence.tags = Some(idsAndWordSpecs.map(idAndSpec => idAndSpec.value.tag))
      sentence.lemmas = Some(idsAndWordSpecs.map(idAndSpec => idAndSpec.value.lemma))
      sentence.entities = Some(idsAndWordSpecs.map(idAndSpec => idAndSpec.value.entity))
      sentence.norms = Some(idsAndWordSpecs.map(idAndSpec => idAndSpec.value.norm))
      sentence.chunks = Some(idsAndWordSpecs.map(idAndSpec => idAndSpec.value.chunk))
      sentence.syntacticTree = None // Documented on Wiki
      sentence.graphs = graphMap
      sentence.relations = None // Documented on Wiki
      new IdAndSentence(sentenceId, sentence)
    }}.getOrElse(List.empty)
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

    // Provenance is always available and therefore extractOpt is not used.
    val sentenceWordPositions = (provenanceValue \ "sentenceWordPositions").extract[JArray].arr.map { intervalValue =>
      deserializeInterval(intervalValue, offset = 1, inclusiveEnd = true)
    }
    require(sentenceWordPositions.size == 1)

    Provenance(document, sentence, sentenceWordPositions.head)
  }

  protected def deserializePluralProvenance(provenanceValue: JValue, documentMap: DocumentMap,
      documentSentenceMap: DocumentSentenceMap): Provenance = {
    val provenanceValues: JArray = provenanceValue.asInstanceOf[JArray]
    require(provenanceValues.arr.nonEmpty)
    // In all cases, the additional provenance is thrown away.  It is recorded for a
    // relation/coreference, but the value is taken directly from the arguments and
    // it is superfluous.
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
    val idAndDctOpt = deserializeDct(nothingToNone((documentValue \ JLDDCT.singular).extractOpt[JValue]))
    // Text is required here!  Can't otherwise make raw for sentences.
    val sentencesSpec = deserializeSentences(documentValue \ "sentences", text)
    val timexCount = sentencesSpec.timexes.map(_.size).sum
    val geolocsCount = sentencesSpec.geolocs.map(_.size).sum
    val sentences = sentencesSpec.sentences
    val document = new Document(sentences)
    document.id = title
    document.text = text
    idAndDctOpt.map(_.value).foreach { dct =>
      DctDocumentAttachment.setDct(document, dct)
    }

    val idAndDocument = new IdAndDocument(documentId, document)
    DocumentSpec(idAndDocument, idAndDctOpt, sentencesSpec)
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

      argumentsValues.arr.map { argumentsValue =>
        requireType(argumentsValue, JLDArgument.typename)
        val name = (argumentsValue \ "type").extract[String]
        val argumentId = id(argumentsValue \ "value")

        if (argumentMap.contains(name)) {
          val oldValues = argumentMap(name)
          // It is done this way to keep the values in order
          val newValues = oldValues :+ argumentId

          argumentMap += name -> newValues
        }
        else
          argumentMap += name -> List(argumentId)
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
      timexMap: TimexMap, geolocMap: GeolocMap, dctMap: DctMap): Attachment = {
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
        require(provenanceOpt.isDefined)
        new Hedging(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "NEGATION" =>
         require(provenanceOpt.isDefined)
        new Negation(text, quantifiers, provenanceOpt, quantifierProvenances)
      case "TIMEX" =>
        // Both of these fall under TIMEX.
        require(provenanceOpt.isEmpty)
        val value = id((stateValue \ "value").extract[JValue])
        require(timexMap.contains(value) || dctMap.contains(value))
        if (timexMap.contains(value))
          new Time(timexMap(value))
        else
          new DCTime(dctMap(value))
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
      timexMap: TimexMap, geolocMap: GeolocMap, dctMap: DctMap): Set[Attachment] = {
    val attachments = statesValueOpt.map { statesValue =>
      statesValue.arr.map { stateValue =>
        deserializeState(stateValue, documentMap, documentSentenceMap, timexMap, geolocMap, dctMap)
      }.toSet
    }.getOrElse(Set.empty)

    attachments
  }

  def deserializeMention(extractionValue: JValue, extraction: Extraction, mentionMap: Map[String, Mention],
      documentMap: DocumentMap, documentSentenceMap: DocumentSentenceMap, timexMap: TimexMap,
      geolocMap: GeolocMap, provenanceMap: ProvenanceMap, dctMap: DctMap): Mention = {
    requireType(extractionValue, JLDExtraction.typename)
    val extractionType = extraction.extractionType
    val extractionSubtype = extraction.extractionSubtype
    val labels = (extractionValue \ "labels").extract[List[String]]
    val tokenInterval = extraction.provenance.interval
    val sentence = extraction.provenance.sentence
    val document = extraction.provenance.document
    val keep = true // Documented on Wiki
    val foundBy = (extractionValue \ "rule").extract[String]
    val paths: Map[String, Map[Mention, SynPath]] = Map.empty // Documented on Wiki
    val misnamedArguments: Map[String, Seq[Mention]] = extraction.argumentMap.map { case (name, ids) =>
      name -> ids.map { id => mentionMap(id) }
    }
    val attachments: Set[Attachment] = deserializeStates((extractionValue \ "states").extractOpt[JArray],
        documentMap, documentSentenceMap, timexMap, geolocMap, dctMap)
    val triggerOpt: Option[TextBoundMention] = extraction.triggerProvenanceOpt.map { provenance =>
      val triggerSentence = provenance.sentence
      val triggerDocument = provenance.document

      new TextBoundMention(labels, provenance.interval, triggerSentence, triggerDocument, keep, foundBy) // See GraphPattern, line 100.
      //      mentionMap(provenanceMap(provenance)).asInstanceOf[TextBoundMention]
    }
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
            "cause" -> misnamedArguments("source"),
            "effect" -> misnamedArguments("destination")
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
            "cause" -> Seq(misnamedArguments("argument").head),
            "effect" -> Seq(misnamedArguments("argument")(1))
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
          val anchor = misnamedArguments("anchor").head
          val neighbor = misnamedArguments("reference").head
          // These arguments seem to get duplicated from the anchor and neighbor.
          // See org.clulab.odin.impl.CrossSentenceExtractor.mkMention.
          val renamedArguments: Map[String, Seq[Mention]] = Map(
            "cause" -> misnamedArguments("anchor"),
            "effect" -> misnamedArguments("reference")
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
      timexMap: TimexMap, geolocMap: GeolocMap, dctMap: DctMap): Map[String, Mention] = {

    // TODO don't pass so many variables, only ones that change!
    def isRipe(extraction: Extraction): Boolean = {
      val hasAllArguments = extraction.argumentMap.forall { case (_, mentionIds) =>
        mentionIds.forall { mentionId =>
          mentionMap.contains(mentionId)
        }
      }
//      val hasTrigger = extraction.triggerProvenanceOpt.forall(provenanceMap.contains)

      hasAllArguments // && hasTrigger
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
            timexMap, geolocMap, provenanceMap, dctMap)

        new IdAndMention(extraction.id, mention)
      }
      val newMentionMap = mentionMap ++ IdAndValue.toMap[Mention](idsAndMentions)
      val newProvenanceMap = provenanceMap ++ ripeExtractions.zip(idsAndMentions).map { case (extraction, idAndMention) =>
        extraction.provenance -> idAndMention.id
      }

      deserializeMentions(extractionsValue, unripeExtractions, newMentionMap, newProvenanceMap,
          documentMap, documentSentenceMap, timexMap, geolocMap, dctMap)
    }
  }

  protected def removeTriggerOnlyMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val argumentMentions = mentions.flatMap { mention => mention.arguments.values.flatten }
    val triggerMentions = mentions.collect {
      case eventMention: EventMention => eventMention.trigger
    }
    val triggerOnlyMentions = triggerMentions.filter { triggerMention =>
      !argumentMentions.exists { argumentMention =>
        triggerMention.eq(argumentMention) // eq is important here
      }
    }
    val remainingMentions = mentions.filter { mention =>
      !triggerOnlyMentions.exists { triggerOnlyMention =>
        mention.eq(triggerOnlyMention)
      }
    }

    remainingMentions
  }

  def deserializeCorpus(corpusValue: JValue, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounding): Corpus = {
    requireType(corpusValue, JLDCorpus.typename)
    // A corpus with no documents is hardly a corpus, so no extractOpt is used (for now).
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
    val dctMap: Map[String, DCT] = documentSpecs.flatMap { documentSpec =>
      documentSpec.idAndDctOpt.map { idAndDct =>
        idAndDct.id -> idAndDct.value
      }
    }.toMap
    val extractionsValueOpt = (corpusValue \ "extractions").extractOpt[JArray]
    val extractions = extractionsValueOpt.map { extractionsValue =>
      extractionsValue.arr.map { extractionValue =>
        deserializeExtraction(extractionValue, documentMap, documentSentenceMap)
      }
    }.getOrElse(Seq.empty[Extraction])
    val mentionMap = extractionsValueOpt.map { extractionsValue =>
      deserializeMentions(extractionsValue, extractions, Map.empty, Map.empty,
        documentMap, documentSentenceMap, timexMap, geolocMap, dctMap)
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
