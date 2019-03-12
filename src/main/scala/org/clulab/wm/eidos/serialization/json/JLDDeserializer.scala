package org.clulab.wm.eidos.serialization.json

import java.time.LocalDateTime

import org.clulab.odin.Attachment
import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
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
import org.clulab.wm.eidos.attachments.EidosAttachment
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
import org.clulab.timenorm.formal.{Interval => TInterval}
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.TimeInterval
import org.clulab.wm.eidos.document.TimeStep
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.Seq


class IdAndValue[ValueType](val id: String, val value: ValueType)

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

class IdAndDocument(id: String, value: Document) extends IdAndValue(id, value)

case class DocumentSpec(idAndDocument: IdAndDocument, sentencesSpec: SentencesSpec)


// Above is for the document, belo is for the extractions

case class StateData(stateType: String, text: String, documentId: String, sentenceId: String, start: Int, end: Int,
    timeValue: Option[String], geoValue: Option[String], modifierDatas: Seq[ModifierData])

case class ModifierData(text: String, documentId: String, sentenceId: String, start: Int, end: Int)




//EidosTextBoundMention -> TextBoundMention -> JLDConceptEntity -> concept/entity
//EidosEventMention -> EventMention -> JLDRelation -> relation/causation relation/correlation relation/coreference
//EidosCrossSentenceMention -> CrossSentenceMention -> JLDRelation
//EidosRelationMention -> RelationMention // These are skipped

class JLDDeserializer {
  implicit val formats = org.json4s.DefaultFormats

  protected def requireType(jValue: JValue, typeName: String): Unit =
      require((jValue \ "@type").extract[String] == typeName)

  protected def id(jValue: JValue): String =
      (jValue \ "@id").extract[String]

  protected def toInterval(start: Int, stop: Int, offset: Int, inclusiveEnd: Boolean): Interval = {
    // Coordinate this with JLDSerializer.
    val endCorrection = if (inclusiveEnd) -1 else 0
    Interval(start - offset, stop - offset - endCorrection)
  }

  def deserializeDct(dctValue: JValue): IdAndDct = {
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

  def deserializeTimeInterval(timeIntervalValue: JValue): TimeStep = {
    requireType(timeIntervalValue, JLDTimeInterval.typename)
    val timeIntervalId = id (timeIntervalValue) // This is never used, so why do we have it?
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

      val idsAndTimexes = (sentenceValue \ "timexes").extract[JArray].arr.map(deserializeTimex)
      timexes = timexes :+ idsAndTimexes.map(_.value)
      timexMap = timexMap ++ idsAndTimexes.map { idAndTimex => idAndTimex.id -> idAndTimex.value }

      val idsAndGeolocs = (sentenceValue \ "geolocs").extract[JArray].arr.map(deserializeGeoloc)
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
      sentence.syntacticTree = None
      sentence.graphs = GraphMap(Map(GraphMap.UNIVERSAL_ENHANCED -> graph))
      sentence.relations = None
      new IdAndSentence(sentenceId, sentence)
    }

    val sentences = idsAndSentences.map(_.value).toArray // Document needs an Array[Sentence]
    // This is because Mention only uses index of sentence in document, not reference to Sentence itself.
    val sentenceMap = idsAndSentences.indices.map { index => idsAndSentences(index).id -> index }.toMap

    SentencesSpec(sentences, sentenceMap, timexes.toArray, timexMap, geolocs.toArray, geolocMap)
  }

  // probably need sentence and document maps, maybe sentence only because that is unique
  def deserializeExtractions(jldExtractionValue: JValue): (Seq[Mention], Seq[EidosMention]) = {
    requireType(jldExtractionValue, "Extraction") // Reference the serializer here?
    val extractionId = id(jldExtractionValue)
    val extractionType = (jldExtractionValue \ "type").extract[String]
    val extractionSubtype = (jldExtractionValue \ "subtype").extract[String]
    val labels = (jldExtractionValue \ "labels").extract[List[String]]
    val tokenIntervalStart = (jldExtractionValue \ "provenance" \ "sentenceWordPositions" \ "start").extract[Int]
    val tokenIntervalStop = (jldExtractionValue \ "provenance" \ "sentenceWordPositions" \ "end").extract[Int]
    val tokenInterval = toInterval(tokenIntervalStart, tokenIntervalStop, offset = 1, inclusiveEnd = true)
    val sentence = 5
    // Need sentence integer in right document from provement
    val document: Document = null // so this needs to be tracked down
    val rule = (jldExtractionValue \ "rule").extract[String]

    val stateDatas = (jldExtractionValue \ "state").extract[JArray].arr.map { jldStatesValue =>
      requireType(jldStatesValue, "State")
      val stateType = (jldStatesValue \ "type").extract[String]
      val text = (jldStatesValue \ "text").extract[String]
      val documentId = id(jldStatesValue \ "provenance" \ "document")
      val sentenceId = id(jldStatesValue \ "provenance" \ "sentence")
      val start = (jldStatesValue \ "provenance" \ "positions" \ "start").extract[Int]
      val end = (jldStatesValue \ "provenance" \ "positions" \ "end").extract[Int]

      val timeValue = (jldStatesValue \ "value" \ "@id").extractOpt[String]
      val geoValue = (jldStatesValue \ "value" \ "@id").extractOpt[String]

      val modifierDatas = (jldStatesValue \ "modifiers").extract[JArray].arr.map { jldModifersValue =>
        requireType(jldModifersValue, "Modifier")
        val text = (jldModifersValue \ "text").extract[String]
        // Provenance is unavailable, so can't track these down
        val documentId = id(jldModifersValue \ "provenance" \ "document")
        val sentenceId = id(jldModifersValue \ "provenance" \ "sentence")
        val start = (jldModifersValue \ "provenance" \ "sentenceWordPositions" \ "start").extract[Int]
        val end = (jldModifersValue \ "provenance" \ "sentenceWordPositions" \ "end").extract[Int]

        ModifierData(text, documentId, sentenceId, start, end)
      }
      StateData(stateType, text, documentId, sentenceId, start, end, timeValue, geoValue, modifierDatas)
    }

/*
    // Need to track down all the mentions now
    val attachments = stateDatas.map { stateData => stateData.stateType match {
      // use the text to find the triggerMention?
        // for quantifiers look at trigger and check its arguments,

      case "QUANT" =>
        val trigger = stateData.text
        val quantifiers = stateData..modifierDatas.map(_.text)

        new Quantification(stateData.text, triggerMention = None, quantifierMentions = None)
      case "INC" => new Increase()
      case "DEC" => new Decrease()

      case "PROP" => new Property()
      case "HEDGE" => new Hedging()
      case "NEGATION" => new Negation()
      case "TIMEX" => new Time()
      case "LocationExp" => new Location()
    }.toSet

    val arguments: Map[String, Seq[Mention]] = null
    val paths: Map[String, Map[Mention, SynPath]] = Map.empty

    val attachments: Set[Attachment] = Set.empty

    if (extractionType == "relation") {
      if (extractionSubtype == "correlation")
        new RelationMention(labels, tokenInterval, arguments, paths, sentence, document, keep = true, rule, attachments)
      else if (extractionSubtype == "causation") {
        val trigger: TextBoundMention = null
        new EventMention(labels, tokenInterval, trigger, arguments, paths, sentence, document, keep = true, rule, attachments)
      }
      else if (extractionSubtype == "coreference") {
        val anchor: Mention = null
        val neighbor: Mention = null
        new CrossSentenceMention(labels, anchor, neighbor, arguments, document, keep = true, rule, attachments)
      }
      else
        throw new Exception("Unknwon kind of relation")
    }
    else if (extractionType == "concept") {
      if (extractionSubtype == "entity")
        new TextBoundMention(labels, tokenInterval, sentence, document, keep = true, rule, attachments)
      else
        throw new Exception("Unknown kind of concept")
    }
    else
      throw new Exception("Unknown kind of extraction")


*/
    (Seq.empty, Seq.empty)
  }

  def deserializeDocument(documentValue: JValue): DocumentSpec = {
    requireType(documentValue, JLDDocument.typename)
    val documentId = id(documentValue)
    val title = (documentValue \ "title").extractOpt[String]
    val text = (documentValue \ "text").extractOpt[String]
    val idAndDct = deserializeDct(documentValue \ "dct")

    val sentencesSpec = deserializeSentences(documentValue \ "sentences")
    val sentences = sentencesSpec.sentences

    val document = new EidosDocument(sentences, text)
    document.id = title
    document.times = Option(sentencesSpec.timexes) // If empty or missing?
    document.geolocs = Option(sentencesSpec.geolocs)
    document.dct = Option(idAndDct.value)

    val idAndDocument = new IdAndDocument(documentId, Document(document))
    DocumentSpec(idAndDocument, sentencesSpec)
  }

  def deserializeCorpus(jldCorpusValue: JValue): Corpus = {
    requireType(jldCorpusValue, JLDCorpus.typename)
    val documentSpecs = (jldCorpusValue \ "documents").extract[JArray].arr.map(deserializeDocument)
    val documentMap = documentSpecs.map { documentSpec =>
      documentSpec.idAndDocument.id -> documentSpec.idAndDocument.value
    }.toMap
    // Maps DocumentId, then SentenceId to Int of sentence within document
    // Is this really necessary?
    val sentenceSpecMap: Map[String, Map[String, Int]] = documentSpecs.map { documentSpec =>
      documentSpec.idAndDocument.id -> documentSpec.sentencesSpec.sentenceMap
    }.toMap
    // Combine all the timexes and geolocs of all corpora
    val timexMap = documentSpecs.flatMap { documentSpec =>
      documentSpec.sentencesSpec.timexMap
    }.toMap
    val geolocMap = documentSpecs.flatMap { documentSpec =>
      documentSpec.sentencesSpec.geolocMap
    }.toMap

// needs to have all the maps go along

    val mentions: List[(Mention, EidosMention)] = (jldCorpusValue \ "extractions").extract[JArray].arr.map { extractionValue =>
//      val (odinMention, eidosMention) = (null // deserializeExtraction(extractionValue)
      (null, null)
    }
    // Need to know which mentions belong to this document and which to others
    // So have to collect them somewhere, documentSpec has extractions for that particular document?
    val odinMentions = mentions.map(_._1)
    val eidosMentions = mentions.map(_._2)

    val annotatedDocuments = documentSpecs.map { documentSpec =>
      AnnotatedDocument(documentSpec.idAndDocument.value, odinMentions, eidosMentions)
    }
    val corpus = annotatedDocuments

    corpus
  }

  def deserialize(json: String): Corpus = {
    val jValue: JValue = parse(json)
    val corpus = deserializeCorpus(jValue)
    corpus
  }
}
