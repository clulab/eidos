package org.clulab.wm.eidos.serialization.json

import java.time.LocalDateTime

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.json4s.jackson.JsonMethods._
import JLDDeserializer.DocumentMap
import JLDDeserializer.DocumentSentenceMap
import org.clulab.struct.Interval
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.TimeInterval
import org.clulab.wm.eidos.document.TimeStep
import org.json4s.JArray

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

  ignore should "deserialize all from jsonld" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))

    val corpus = new JLDDeserializer().deserialize(json, ieSystem.canonicalizer, ieSystem.loadableAttributes.multiOntologyGrounder)
  }

  it should "deserialize DCT from jsonld" in {
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

  it should "deserialize TimeInterval from jsonld" in {
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

  it should "deserialize TimexExpression from jsonld" in {
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

  it should "deserialize GeoLocation from jsonld" in {
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

  it should "deserialize Word from jsonld" in {
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

  it should "deserialize Dependency from jsonld" in {
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

  it should "deserialize Sentence from jsonld" in {
    val json = """
      |[ {
      |  "@type" : "Sentence",
      |  "@id" : "_:Sentence_1",
      |  "text" : "Contents .",
      |  "words" : [ {
      |    "@type" : "Word",
      |    "@id" : "_:Word_1",
      |    "text" : "Contents",
      |    "tag" : "NNS",
      |    "entity" : "O",
      |    "startOffset" : 0,
      |    "endOffset" : 8,
      |    "lemma" : "contents",
      |    "chunk" : "B-NP",
      |    "norm" : "O"
      |  }, {
      |    "@type" : "Word",
      |    "@id" : "_:Word_2",
      |    "text" : ".",
      |    "tag" : ".",
      |    "entity" : "O",
      |    "startOffset" : 8,
      |    "endOffset" : 9,
      |    "lemma" : ".",
      |    "chunk" : "O",
      |    "norm" : "O"
      |  } ],
      |  "dependencies" : [ {
      |    "@type" : "Dependency",
      |    "source" : {
      |      "@id" : "_:Word_1"
      |    },
      |    "destination" : {
      |      "@id" : "_:Word_2"
      |    },
      |    "relation" : "punct"
      |  } ]
      |}, {
      |  "@type" : "Sentence",
      |  "@id" : "_:Sentence_2",
      |  "text" : "IntErnatIonal cErEal PrIcES ...............",
      |  "words" : [ {
      |    "@type" : "Word",
      |    "@id" : "_:Word_3",
      |    "text" : "IntErnatIonal",
      |    "tag" : "JJ",
      |    "entity" : "O",
      |    "startOffset" : 11,
      |    "endOffset" : 24,
      |    "lemma" : "international",
      |    "chunk" : "B-NP",
      |    "norm" : "O"
      |  }, {
      |    "@type" : "Word",
      |    "@id" : "_:Word_4",
      |    "text" : "cErEal",
      |    "tag" : "NNP",
      |    "entity" : "O",
      |    "startOffset" : 25,
      |    "endOffset" : 31,
      |    "lemma" : "cErEal",
      |    "chunk" : "I-NP",
      |    "norm" : "O"
      |  }, {
      |    "@type" : "Word",
      |    "@id" : "_:Word_5",
      |    "text" : "PrIcES",
      |    "tag" : "NNP",
      |    "entity" : "B-Property",
      |    "startOffset" : 32,
      |    "endOffset" : 38,
      |    "lemma" : "PrIcES",
      |    "chunk" : "I-NP",
      |    "norm" : "O"
      |  }, {
      |    "@type" : "Word",
      |    "@id" : "_:Word_6",
      |    "text" : "...............",
      |    "tag" : "NNP",
      |    "entity" : "O",
      |    "startOffset" : 39,
      |    "endOffset" : 54,
      |    "lemma" : "...............",
      |    "chunk" : "I-NP",
      |    "norm" : "O"
      |  } ],
      |  "dependencies" : [ {
      |    "@type" : "Dependency",
      |    "source" : {
      |      "@id" : "_:Word_6"
      |    },
      |    "destination" : {
      |      "@id" : "_:Word_5"
      |    },
      |    "relation" : "compound"
      |  }, {
      |    "@type" : "Dependency",
      |    "source" : {
      |      "@id" : "_:Word_6"
      |    },
      |    "destination" : {
      |      "@id" : "_:Word_3"
      |    },
      |    "relation" : "amod"
      |   }, {
      |    "@type" : "Dependency",
      |    "source" : {
      |      "@id" : "_:Word_6"
      |    },
      |    "destination" : {
      |      "@id" : "_:Word_4"
      |    },
      |    "relation" : "compound"
      |  } ]
      |} ]""".stripMargin
    val sentencesValue = parse(json)
    val sentenceSpec = new JLDDeserializer().deserializeSentences(sentencesValue)
  }

  it should "deserialize Interval from jsonld" in {
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

  it should "deserialize Provenance from jsonld" in {
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
    val provenance = new JLDDeserializer().deserializeProvenance(Option(provenanceValue), documentMap, documentSentenceMap)
  }

  it should "deserialize Trigger from jsonld" in {
    val json = """
      |{
      |  "@type" : "Trigger",
      |  "text" : "Due",
      |  "provenance" : [ {
      |    "@type" : "Provenance",
      |    "document" : {
      |      "@id" : "_:Document_1"
      |    },
      |    "documentCharInterval" : [ {
      |      "@type" : "Interval",
      |      "start" : 3232,
      |      "end" : 3234
      |    } ],
      |    "sentence" : {
      |      "@id" : "_:Sentence_35"
      |    },
      |    "sentenceWordPositions" : [ {
      |      "@type" : "Interval",
      |      "start" : 1,
      |      "end" : 1
      |    } ]
      |  } ]
      |}""".stripMargin
    val triggerValue = Option(parse(json))
    val documentMap: DocumentMap = Map("_:Document_1" -> null)
    val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_35" -> 0))
    val provenanceOpt = new JLDDeserializer().deserializeTrigger(triggerValue, documentMap, documentSentenceMap)
  }

  it should "deserialize Extraction from jsonld" in {
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

  it should "deserialize Modifiers from jsonld" in {
    val json = """
      |[ {
      |  "@type" : "Modifier",
      |  "text" : "sharply",
      |  "provenance" : [ {
      |    "@type" : "Provenance",
      |    "document" : {
      |    "@id" : "_:Document_1"
      |    },
      |    "documentCharInterval" : [ {
      |    "@type" : "Interval",
      |    "start" : 14025,
      |    "end" : 14031
      |    } ],
      |    "sentence" : {
      |    "@id" : "_:Sentence_253"
      |    },
      |    "sentenceWordPositions" : [ {
      |    "@type" : "Interval",
      |    "start" : 18,
      |    "end" : 18
      |    } ]
      |  } ],
      |  "intercept" : 0.5958,
      |  "mu" : 1.034E-5,
      |  "sigma" : -0.001123
      |} ]""".stripMargin
    val modifiersValue = parse(json).asInstanceOf[JArray]
    val documentMap: DocumentMap = Map("_:Document_1" -> null)
    val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_253" -> 0))
    val (textsOpt, provenancesOpt) = new JLDDeserializer().deserializeModifiers(Option(modifiersValue), documentMap, documentSentenceMap)
  }

  it should "deserialize States from jsonld" in {
    val json = """
      |[ {
      |  "@type" : "State",
      |  "type" : "DEC",
      |  "text" : "decreased",
      |  "provenance" : [ {
      |    "@type" : "Provenance",
      |    "document" : {
      |      "@id" : "_:Document_1"
      |    },
      |    "documentCharInterval" : [ {
      |      "@type" : "Interval",
      |      "start" : 14015,
      |      "end" : 14023
      |    } ],
      |    "sentence" : {
      |      "@id" : "_:Sentence_253"
      |    },
      |    "sentenceWordPositions" : [ {
      |      "@type" : "Interval",
      |      "start" : 17,
      |      "end" : 17
      |    } ]
      |  } ],
      |  "modifiers" : [ {
      |    "@type" : "Modifier",
      |    "text" : "sharply",
      |    "provenance" : [ {
      |      "@type" : "Provenance",
      |      "document" : {
      |        "@id" : "_:Document_1"
      |      },
      |      "documentCharInterval" : [ {
      |        "@type" : "Interval",
      |        "start" : 14025,
      |        "end" : 14031
      |      } ],
      |      "sentence" : {
      |        "@id" : "_:Sentence_253"
      |      },
      |      "sentenceWordPositions" : [ {
      |        "@type" : "Interval",
      |        "start" : 18,
      |        "end" : 18
      |      } ]
      |    } ],
      |    "intercept" : 0.5958,
      |    "mu" : 1.034E-5,
      |    "sigma" : -0.001123
      |  } ]
      |}, {
      |  "@type" : "State",
      |  "type" : "PROP",
      |  "text" : "prices"
      |}, {
      |  "@type" : "State",
      |  "type" : "TIMEX",
      |  "text" : "December 2015.",
      |  "value" : {
      |    "@id" : "_:DCT_1"
      |  }
      |} ]""".stripMargin
    val statesValue = parse(json).asInstanceOf[JArray]
    val documentMap: DocumentMap = Map("_:Document_1" -> null)
    val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_253" -> 0))
    val timeIntervel = TimeInterval(Interval(0, 4), List.empty[TimeStep], "hello there")
    val timexMap = Map("_:DCT_1" -> timeIntervel)
    val geolocMap = Map.empty[String, GeoPhraseID]
    val attachments = new JLDDeserializer().deserializeStates(Option(statesValue), documentMap, documentSentenceMap,
        timexMap, geolocMap)
  }

  it should "deserialize Arguments from jsonld" in {
    val json = """
      |[ {
      |  "@type" : "Argument",
      |  "type" : "source",
      |  "value" : {
      |    "@id" : "_:Extraction_145"
      |  }
      |}, {
      |  "@type" : "Argument",
      |  "type" : "destination",
      |  "value" : {
      |    "@id" : "_:Extraction_86"
      |  }
      |} ]""".stripMargin
    val argumentsValue = parse(json).asInstanceOf[JArray]
    val argumentMap = new JLDDeserializer().deserializeArguments(Option(argumentsValue))
  }

  // deserializeMention TODO

  // deserializeMentions TODO

  // TODO deserializeDocument

  // deserializeCorpus // essentially the whole thing

}
