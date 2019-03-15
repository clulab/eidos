package org.clulab.wm.eidos.serialization.json

import java.time.LocalDateTime

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.json4s.jackson.JsonMethods._

import JLDDeserializer.DocumentMap
import JLDDeserializer.DocumentSentenceMap

import scala.collection.Seq

class TestJLDDeserializer extends ExtractionTest {
  val adjectiveGrounder = EidosAdjectiveGrounder.fromConfig(ieSystem.config.getConfig("adjectiveGrounder"))

  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val annotatedDocument = ieSystem.extractFromText(text, keepText = true)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus) = {
    val json = {
      val jldCorpus = new JLDEidosCorpus(corpus)
      val jValue = jldCorpus.serialize(adjectiveGrounder)
      stringify(jValue, true)
    }
    
    json
  }

  def inspect(string: String): Unit =
      if (false) println(string)
  
  behavior of "JLDDeserializer"

  ignore should "deserialize" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))

    val corpus = new JLDDeserializer().deserialize(json)
  }

  it should "deserialize a DCT from json" in {
    val json = """
      |{
      |  "@type" : "DCT",
      |  "@id" : "_:DCT_1",
      |  "text" : "February 2017.",
      |  "start" : "2017-02-01T00:00",
      |  "end" : "2017-03-01T00:00"
      |}""".stripMargin
    val dctValue = parse(json)
    val idAndDct = new JLDDeserializer().deserializeDct(dctValue)
    val id = idAndDct.id
    val dct = idAndDct.value

    id should be ("_:DCT_1")
    dct.text should be ("February 2017.")
    dct.interval.start.toString should be ("2017-02-01T00:00")
    dct.interval.end.toString should be ("2017-03-01T00:00")
  }

  it should "deserialize a TimeInterval from json" in {
    val json = """
      |{
      |  "@type" : "TimeInterval",
      |  "@id" : "_:TimeInterval_5",
      |  "start" : "2017-01-01T00:00",
      |  "end" : "2017-02-01T00:00",
      |  "duration" : 2678400
      |}""".stripMargin
    val timeIntervalValue = parse(json)
    val timeStep = new JLDDeserializer().deserializeTimeInterval(timeIntervalValue)

    timeStep.start should be (LocalDateTime.parse("2017-01-01T00:00"))
    timeStep.end should be (LocalDateTime.parse("2017-02-01T00:00"))
    timeStep.duration should be (2678400)
  }

  it should "deserialize a Timex from json" in {
    val json = """
      |{
      |  "@type" : "TimeExpression",
      |  "@id" : "_:TimeExpression_5",
      |  "startOffset" : 3217,
      |  "endOffset" : 3229,
      |  "text" : "January 2017",
      |  "intervals" : [ {
      |    "@type" : "TimeInterval",
      |    "@id" : "_:TimeInterval_5",
      |    "start" : "2017-01-01T00:00",
      |    "end" : "2017-02-01T00:00",
      |    "duration" : 2678400
      |  } ]
      |}""".stripMargin
    val timexValue = parse(json)
    val idAndTimex = new JLDDeserializer().deserializeTimex(timexValue)

    idAndTimex.id should be ("_:TimeExpression_5")
  }

  it should "deserialize a geoloc from json" in {
    val json = """
      |{
      |  "@type" : "GeoLocation",
      |  "@id" : "_:GeoLocation_15",
      |  "startOffset" : 3612,
      |  "endOffset" : 3623,
      |  "text" : "South Sudan",
      |  "geoID" : "7909807"
      |}""".stripMargin
    val geoLocValue = parse(json)
    val idAndGeoPhraseID = new JLDDeserializer().deserializeGeoloc(geoLocValue)
    val id = idAndGeoPhraseID.id
    val geoPhraseID = idAndGeoPhraseID.value

    id should be ("_:GeoLocation_15")
    geoPhraseID.geonameID should be (Some(7909807))
  }

  it should "deserialize WordData from json" in {
    val json = """
      |{
      |  "@type" : "Word",
      |  "@id" : "_:Word_19",
      |  "text" : "Famine",
      |  "tag" : "NN",
      |  "entity" : "O",
      |  "startOffset" : 78,
      |  "endOffset" : 84,
      |  "lemma" : "famine",
      |  "chunk" : "B-NP",
      |  "norm" : "O"
      |}""".stripMargin
    val wordDataValue = parse(json)
    val idAndWordData = new JLDDeserializer().deserializeWordData(wordDataValue)
    val id = idAndWordData.id
    val wordData = idAndWordData.value

    id should be ("_:Word_19")
  }

  it should "deserialize dependency from json" in {
    val json = """
      |{
      |  "@type" : "Dependency",
      |  "source" : {
      |    "@id" : "_:Word_2"
      |  },
      |  "destination" : {
      |     "@id" : "_:Word_1"
      |  },
      |  "relation" : "compound"
      |}""".stripMargin
    val wordMap = Map("_:Word_2" -> 2, "_:Word_1" -> 1)
    val dependencyValue = parse(json)
    val dependency = new JLDDeserializer().deserializeDependency(dependencyValue, wordMap)
  }

  it should "deserialize interval from json" in {
    val json = """
      |{
      |  "@type" : "Interval",
      |  "start" : 3232,
      |  "end" : 3234
      |}"""".stripMargin
    val intervalValue = parse(json)
    val interval = new JLDDeserializer().deserializeInterval(intervalValue, offset = 1, inclusiveEnd = true)

    interval.start should be (3231)
    interval.end should be (3234)
  }

  it should "deserialize provenance from json" in {
    val json = """
    |[ {
    |  "@type" : "Provenance",
    |  "document" : {
    |    "@id" : "_:Document_1"
    |  },
    |  "documentCharInterval" : [ {
    |    "@type" : "Interval",
    |    "start" : 3232,
    |    "end" : 3234
    |  } ],
    |  "sentence" : {
    |    "@id" : "_:Sentence_35"
    |  },
    |  "sentenceWordPositions" : [ {
    |    "@type" : "Interval",
    |    "start" : 1,
    |    "end" : 1
    |  } ]
    |} ]"""".stripMargin
    val provenanceValue = parse(json)
    val documentMap: DocumentMap = Map("_:Document_1" -> null)
    val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_35" -> 34))
    val provenance = new JLDDeserializer().deserializeProvenance(provenanceValue, documentMap, documentSentenceMap)
  }

  it should "deserialize extraction from json" in {
    val json = """
      |{
      |  "@type" : "Extraction",
      |  "@id" : "_:Extraction_1",
      |  "type" : "relation",
      |  "subtype" : "causation",
      |  "labels" : [ "Causal", "DirectedRelation", "EntityLinker", "Event" ],
      |  "text" : "conflict are also forcing many families to leave South Sudan for neighbouring countries",
      |  "rule" : "ported_syntax_1_verb-Causal",
      |  "canonicalName" : "conflict force leave",
      |  "provenance" : [ {
      |    "@type" : "Provenance",
      |    "document" : {
      |      "@id" : "_:Document_1"
      |    },
      |    "documentCharPositions" : [ {
      |      "@type" : "Interval",
      |      "start" : 1559,
      |      "end" : 1645
      |    } ],
      |    "sentence" : {
      |      "@id" : "_:Sentence_7"
      |    },
      |    "sentenceWordPositions" : [ {
      |      "@type" : "Interval",
      |      "start" : 4,
      |      "end" : 16
      |    } ]
      |  } ],
      |  "trigger" : {
      |    "@type" : "Trigger",
      |    "text" : "forcing",
      |    "provenance" : [ {
      |      "@type" : "Provenance",
      |      "document" : {
      |        "@id" : "_:Document_1"
      |      },
      |      "documentCharPositions" : [ {
      |        "@type" : "Interval",
      |        "start" : 1577,
      |        "end" : 1583
      |      } ],
      |      "sentence" : {
      |        "@id" : "_:Sentence_7"
      |      },
      |      "sentenceWordPositions" : [ {
      |        "@type" : "Interval",
      |        "start" : 7,
      |        "end" : 7
      |      } ]
      |    } ]
      |  },
      |  "arguments" : [ {
      |    "@type" : "Argument",
      |    "type" : "source",
      |    "value" : {
      |      "@id" : "_:Extraction_7"
      |    }
      |  }, {
      |    "@type" : "Argument",
      |    "type" : "destination",
      |    "value" : {
      |      "@id" : "_:Extraction_8"
      |    }
      |  } ]
      |}""".stripMargin
    val extractionValue = parse(json)
    val documentMap: DocumentMap = Map("_:Document_1" -> null)
    val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_7" -> 0))
    val extraction = new JLDDeserializer().deserializeExtraction(extractionValue, documentMap, documentSentenceMap)

  }

//  it should "deserialize trigger from json" in {
//    val json = """
//      |{
//      |  "@type" : "Trigger",
//      |  "text" : "Due",
//      |  "provenance" : [ {
//      |    "@type" : "Provenance",
//      |    "document" : {
//      |      "@id" : "_:Document_1"
//      |    },
//      |    "documentCharInterval" : [ {
//      |      "@type" : "Interval",
//      |      "start" : 3232,
//      |      "end" : 3234
//      |    } ],
//      |    "sentence" : {
//      |      "@id" : "_:Sentence_35"
//      |    },
//      |    "positions" : [ {
//      |      "@type" : "Interval",
//      |      "start" : 1,
//      |      "end" : 1
//      |    } ]
//      |  } ]
//      ||}""".stripMargin
//    val wordMap = Map("_:Word_2" -> 2, "_:Word_1" -> 1)
//    val dependencyValue = parse(json)
//    val dependency = new JLDDeserializer().deserializeDependency(dependencyValue, wordMap)
//  }
}
