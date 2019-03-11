package org.clulab.wm.eidos.serialization.json

import org.clulab.odin.Attachment
import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.EventMention
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
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.Seq

case class WordData(startOffset: Int, endOffset: Int, word: String, tag: String, lemma: String, entity: String,
    norm: String, chunk: String)

case class StateData(stateType: String, text: String, documentId: String, sentenceId: String, start: Int, end: Int,
    timeValue: Option[String], geoValue: Option[String], modifierDatas: Seq[ModifierData])

case class ModifierData(text: String, documentId: String, sentenceId: String, start: Int, end: Int)

case class DocumentData(document: Document) // eventually sentence map

case class IdAndWordData(id: String, wordData: WordData)

case class IdAndSentence(id: String, sentence: Sentence)

case class IdAndDocumentData(id: String, documentData: DocumentData)

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

  // probably need sentence and document maps, maybe sentence only because that is unique
  def deserializeExtractions(jldExtractionValue: JValue): (Seq[Mention], Seq[EidosMention]) = {
    requireType(jldExtractionValue, "Extraction") // Reference the serializer here?
    val extractionId = id(jldExtractionValue)
    val extractionType = (jldExtractionValue \ "type").extract[String]
    val extractionSubtype = (jldExtractionValue \ "subtype").extract[String]
    val labels = (jldExtractionValue \ "labels").extract[List[String]]
    val tokenIntervalStart = (jldExtractionValue \ "provenance" \ "positions" \ "start").extract[Int]
    val tokenIntervalStop = (jldExtractionValue \ "provenance" | "positions" \ "end").extract[Int]
    val tokenInterval = new Interval(tokenIntervalStart - 1, tokenIntervalStop)
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
        val start = (jldModifersValue \ "provenance" \ "positions" \ "start").extract[Int]
        val end = (jldModifersValue \ "provenance" \ "positions" \ "end").extract[Int]

        ModifierData(text, documentId, sentenceId, start, end)
      }
      StateData(stateType, text, documentId, sentenceId, start, end, timeValue, geoValue, modifierDatas)
    }

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



    (Seq.empty, Seq.empty)
  }

  def deserializeDocument(jldDocumentValue: JValue): IdAndDocumentData = {
    requireType(jldDocumentValue, "Document")
    val documentId = id(jldDocumentValue)
    val title = (jldDocumentValue \ "title").extractOpt[String]
    val text = (jldDocumentValue \ "text").extractOpt[String]
    val idsAndSentences = (jldDocumentValue \ "sentences").extract[JArray].arr.map { jldSentenceValue: JValue =>
      requireType(jldSentenceValue, "Sentence")
      val sentenceId = id(jldSentenceValue)
      val raw: Array[String] = (jldSentenceValue \ "text").extract[String].split(' ')

      val idsAndWords: Array[IdAndWordData] = (jldSentenceValue \ "words").extract[JArray].arr.map { jldWordValue: JValue =>
        requireType(jldWordValue, "Word")
        val wordId = id(jldWordValue)
        val text = (jldWordValue \ "text").extract[String]
        val tag = (jldWordValue \ "tag").extract[String]
        val entity = (jldWordValue \ "entity").extract[String]
        val startOffset = (jldWordValue \ "startOffset").extract[Int]
        val endOffset = (jldWordValue \ "endOffset").extract[Int]
        val lemma = (jldWordValue \ "lemma").extract[String]
        val chunk = (jldWordValue \ "chunk").extract[String]
        val norm = (jldWordValue \ "norm").extract[String]
        IdAndWordData(wordId, WordData(startOffset, endOffset, text, tag, lemma, entity, norm, chunk))
      }.toArray
      val wordMap = idsAndWords.indices.map { index => idsAndWords(index).id -> index }.toMap

      val dependencies = (jldSentenceValue \ "dependencies").extract[JArray].arr.map { jldDependencyValue: JValue =>
        requireType(jldDependencyValue, "Dependency")
        val source = id(jldDependencyValue)
        val destination = id(jldDependencyValue)
        val relation = (jldDependencyValue \ "relation").extract[String]
        val edge: Edge[String] = Edge(wordMap(source), wordMap(destination), relation)

        edge
      }
      val sourceRoots = dependencies.map(_.source).toSet
      val destinationRoots = dependencies.map(_.destination).toSet
      val roots = sourceRoots ++ destinationRoots
      val graph = DirectedGraph[String](dependencies, roots)

      val startOffsets: Array[Int] = idsAndWords.map(_.wordData.startOffset)
      val endOffsets: Array[Int] = idsAndWords.map(_.wordData.endOffset)
      val words: Array[String] = idsAndWords.map(_.wordData.word)
      val sentence = Sentence(raw, startOffsets, endOffsets, words)
      sentence.tags = Some(idsAndWords.map(_.wordData.tag))
      sentence.lemmas = Some(idsAndWords.map(_.wordData.lemma))
      sentence.entities = Some(idsAndWords.map(_.wordData.entity))
      sentence.norms = Some(idsAndWords.map(_.wordData.norm))
      sentence.chunks = Some(idsAndWords.map(_.wordData.chunk))
      sentence.syntacticTree = None
      sentence.graphs = GraphMap(Map(GraphMap.UNIVERSAL_ENHANCED -> graph))
      sentence.relations = None
      IdAndSentence(sentenceId, sentence)
    }.toArray

    // Just do plain document here, wait for r est?
    val sentences = idsAndSentences.map(_.sentence)
    val document = new EidosDocument(sentences, text)
    document.id = title
    document.dct = None // Do try to read this
    document.times = None // read these
    document.geolocs = None // read these

    // Need sentence map for provenance or
    IdAndDocumentData(documentId, DocumentData(document)) // e.g., sentence map?
  }

  def deserializeCorpus(jldCorpusValue: JValue): Corpus = {
    requireType(jldCorpusValue, "Corpus")
    // What if empty?
    val idAndDocumentDatas = (jldCorpusValue \ "documents").extract[JArray].arr.map { jldDocumentValue: JValue =>
      val documentAndDocumentId = deserializeDocument(jldDocumentValue)
      documentAndDocumentId
    }

    // What if JArray is empty? or doesn't exist?
    val jldExtractionsValue = (jldCorpusValue \ "extractions").extract[JArray].arr.map { jldExtractionValue: JValue =>
        // probably can't get read menion in one pass because they reference each other
        // probably have to return data first time around
      val (odinMention, eidosMention) = deserializeExtractions(jldExtractionsValue)
    }

//    val annotatedDocuments = idAndDocumentDatas.map { idAndDocumentData =>
//      val annotatedDocument = AnnotatedDocument(idAndDocumentData.documentData.document, odinMentions, eidosMentions)
//      annotatedDocument
//
//    val corpus = annotatedDocuments
//    corpus
    null
  }

  def deserialize(json: String): Corpus = {
    val jValue: JValue = parse(json)
    val corpus = deserializeCorpus(jValue)
    corpus
  }
}
