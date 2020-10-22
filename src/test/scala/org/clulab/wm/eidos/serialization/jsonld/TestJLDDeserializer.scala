package org.clulab.wm.eidos.serialization.jsonld

import java.time.LocalDateTime

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.serialization.json.stringify
import org.clulab.struct.Interval
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.DCTime
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Hedging
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.attachments.NegChange
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.attachments.PosChange
import org.clulab.wm.eidos.attachments.Property
import org.clulab.wm.eidos.attachments.Provenance
import org.clulab.wm.eidos.attachments.Quantification
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.groundings.OntologyAliases.MultipleOntologyGrounding
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.PredicateGrounding
import org.clulab.wm.eidos.mentions.CrossSentenceEventMention
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.DctMap
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.DocumentMap
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.DocumentSentenceMap
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.GeolocMap
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.MentionMap
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer.ProvenanceMap
import org.clulab.wm.eidos.test.TestUtils.ContraptionTest
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.test.TestUtils.newEidosSystem
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.clulab.wm.eidos.utils.FileUtils
import org.json4s.JArray
import org.json4s.jackson.JsonMethods._

import scala.collection.Seq

class TestJLDDeserializer extends ExtractionTest {

  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
//    val documentCreationTime: Option[String] = Some("This is a test")
    val documentCreationTime: Option[String] = Some(LocalDateTime.now().toString.take(10))
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = true,
      documentCreationTime, idOpt = None)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }

  def serialize(corpus: Corpus): String = {
    val json = {
      val jldCorpus = new JLDCorpus(corpus)
      val jValue = jldCorpus.serialize()
      stringify(jValue, pretty = true)
    }
    
    json
  }

  def inspect(string: String): Unit =
      if (false) println(string)
  
  behavior of "JLDDeserializer"


  def testParts(): Unit = {
    it should "deserialize DCT from jsonld" in {
      val json = """
        |{
        |  "@type" : "DCT",
        |  "@id" : "_:DCT_1",
        |  "text" : "February 2017.",
        |  "start" : "2017-02-01T00:00",
        |  "end" : "2017-03-01T00:00"
        |}""".stripMargin
    val dctValue = Option(parse(json))
    val idAndDctOpt = new JLDDeserializer().deserializeDct(dctValue)
    val id = idAndDctOpt.get.id
    val dct = idAndDctOpt.get.value

    id should be("_:DCT_1")
    dct.text should be("February 2017.")
    dct.interval.start.toString should be("2017-02-01T00:00")
    dct.interval.end.toString should be("2017-03-01T00:00")
  }

  it should "deserialize TimeInterval from jsonld" in {
    val json =
      """
        |{
        |  "@type" : "TimeInterval",
        |  "@id" : "_:TimeInterval_5",
        |  "start" : "2017-01-01T00:00",
        |  "end" : "2017-02-01T00:00"
        |}""".stripMargin
      val timeIntervalValue = parse(json)
      val timeStep = new JLDDeserializer().deserializeTimeInterval(timeIntervalValue)

      timeStep.startDate should be(LocalDateTime.parse("2017-01-01T00:00"))
      timeStep.endDate should be(LocalDateTime.parse("2017-02-01T00:00"))
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
        |    "end" : "2017-02-01T00:00"
        |  } ]
        |}""".stripMargin
      val timexValue = parse(json)
      val idAndTimex = new JLDDeserializer().deserializeTimex(timexValue)

      idAndTimex.id should be("_:TimeExpression_5")
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

      id should be("_:GeoLocation_15")
      geoPhraseID.geonameID should be(Some("7909807"))
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
//      val wordData = idAndWordData.value

      id should be("_:Word_19")
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
      /*val dependency =*/ new JLDDeserializer().deserializeDependency(dependencyValue, wordMap)
    }

    it should "deserialize Sentence from jsonld" in {
      val documentText = "Contents.  IntErnatIonal cErEal PrIcES ..............."
      val json = """
        |[ {
        |  "@type" : "Sentence",
        |  "@id" : "_:Sentence_1",
        |  "text" : "Contents .",
        |  "relevance" : 0.5,
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
      val sentenceSpec = new JLDDeserializer().deserializeSentences(sentencesValue, Some(documentText))

      sentenceSpec.relevanceOpts.length should be (2)
      sentenceSpec.relevanceOpts.head.isDefined should be (true)
      sentenceSpec.relevanceOpts.head.get should be (0.5f)
      sentenceSpec.relevanceOpts.tail.head.isDefined should be (false)
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

      interval.start should be(3231)
      interval.end should be(3234)
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
      /*val provenance =*/ new JLDDeserializer().deserializeProvenance(Option(provenanceValue), documentMap, documentSentenceMap)
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
      /*val provenanceOpt =*/ new JLDDeserializer().deserializeTrigger(triggerValue, documentMap, documentSentenceMap)
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
        |  "groundings" : [ {
        |    "@type" : "Groundings",
        |    "name" : "wm"
        |  }, {
        |    "@type" : "Groundings",
        |    "name" : "wm_compositional/concept",
        |    "category" : "concept"
        |  } ],
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
      /*val extraction =*/ new JLDDeserializer().deserializeExtraction(extractionValue, documentMap, documentSentenceMap)
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
      /*val (textsOpt, provenancesOpt) =*/ new JLDDeserializer().deserializeModifiers(Option(modifiersValue), documentMap, documentSentenceMap)
    }

    it should "deserialize States from jsonld" in {
      val provenance = """
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
        |""".stripMargin

      val json = s"""
        |[ {
        |  "@type" : "State",
        |  "type" : "QUANT",
        |  "text" : "quantifier",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "INC",
        |  "text" : "increased",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "DEC",
        |  "text" : "decreased",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "PROP",
        |  "text" : "property"
        |}, {
        |  "@type" : "State",
        |  "type" : "HEDGE",
        |  "text" : "hedging",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "NEGATION",
        |  "text" : "negation",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "POS",
        |  "text" : "positive change",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "NEG",
        |  "text" : "negative change",
        |  $provenance
        |}, {
        |  "@type" : "State",
        |  "type" : "LocationExp",
        |  "text" : "text",
        |  "value" : {
        |    "@id" : "_:GeoLocation_1"
        |  }
        |}, {
        |  "@type" : "State",
        |  "type" : "TIMEX",
        |  "text" : "text",
        |  "value" : {
        |    "@id" : "_:TimeExpression_1"
        |  }
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
      val timeIntervel = TimEx(Interval(0, 4), Seq.empty[TimeStep], "hello there")
      val timexMap = Map("_:TimeExpression_1" -> timeIntervel)
      val geoPhraseID = GeoPhraseID("text", Some("Denmark"), 3, 5)
      val geolocMap: GeolocMap = Map("_:GeoLocation_1" -> geoPhraseID)

      val dct = DCT(SimpleInterval(LocalDateTime.now.minusHours(5), LocalDateTime.now), "text")
      val dctMap: DctMap = Map("_:DCT_1" -> dct)
      val attachments = new JLDDeserializer().deserializeStates(Option(statesValue), documentMap, documentSentenceMap,
          timexMap, geolocMap, dctMap)

      attachments.exists { attachment => attachment.isInstanceOf[Quantification]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Increase]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Decrease]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Property]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Hedging]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Negation]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[PosChange]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[NegChange]} should be (true)

      attachments.exists { attachment => attachment.isInstanceOf[Location]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[Time]} should be (true)
      attachments.exists { attachment => attachment.isInstanceOf[DCTime]} should be (true)
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
      /*val argumentMap =*/ new JLDDeserializer().deserializeArguments(Option(argumentsValue))
    }

    it should "deserialize compositional groundings from jsonld" in {
      val json = """
        |[ {
        |  "@type" : "Groundings",
        |  "name" : "wm_compositional",
        |  "version" : "27dea4a3c0665c5c6bdd095e568c1c534d09d1b1",
        |  "versionDate" : "2020-09-07T05:12:11Z",
        |  "values" : [ {
        |	   "@type" : "PredicateGrounding",
        |	   "theme" : [ {
        |	     "@type" : "Grounding",
        |	     "ontologyConcept" : "wm_compositional/concept/service/information/",
        |	     "value" : 0.5029400587081909
        |	   }, {
        |	     "@type" : "Grounding",
        |	     "ontologyConcept" : "wm_compositional/concept/government/government_program",
        |	     "value" : 0.49185845255851746
        |	   }, {
        |	     "@type" : "Grounding",
        |	     "ontologyConcept" : "wm_compositional/concept/physical_market",
        |	     "value" : 0.4598035216331482
        |	   } ],
        |	   "value" : 0.4929400682449341,
        |	   "display" : "THEME: wm_compositional/concept/service/information/"
        |  }, {
        |	   "@type" : "PredicateGrounding",
        |	   "themeProcessProperties" : [ {
        |	     "@type" : "Grounding",
        |	     "ontologyConcept" : "wm_compositional/concept/population/density/",
        |	     "value" : 0.34960877895355225
        |	   }, {
        |	     "@type" : "Grounding",
        |	     "ontologyConcept" : "wm_compositional/concept/migration/returnees",
        |	     "value" : 0.29021745920181274
        |	   } ],
        |	   "value" : 0.3396087884902954,
        |	   "display" : "THEME: wm_compositional/concept/population/density/"
        |  } ]
        |} ]
        |""".stripMargin
      val jValue = parse(json).asInstanceOf[JArray]
      val ontologyGroundings: OntologyGroundings = new JLDDeserializer().deserializeGroundings(jValue)

      ontologyGroundings.size should be (1)

      val ontologyGrounding: OntologyGrounding = ontologyGroundings("wm_compositional")
      val multipleOntologyGrounding: MultipleOntologyGrounding = ontologyGrounding.grounding

      multipleOntologyGrounding.size should be (2)

      val multipleOntologyGrounding0 = multipleOntologyGrounding(0)

      multipleOntologyGrounding0.isInstanceOf[PredicateGrounding] should be (true)
      multipleOntologyGrounding0.asInstanceOf[PredicateGrounding].predicateTuple.theme.grounding.size should be (3)
      multipleOntologyGrounding0.asInstanceOf[PredicateGrounding].predicateTuple.themeProcessProperties.grounding.size should be (0)

      val multipleOntologyGrounding1 = multipleOntologyGrounding(1)

      multipleOntologyGrounding1.isInstanceOf[PredicateGrounding] should be (true)
      multipleOntologyGrounding1.asInstanceOf[PredicateGrounding].predicateTuple.theme.grounding.size should be (0)
      multipleOntologyGrounding1.asInstanceOf[PredicateGrounding].predicateTuple.themeProcessProperties.grounding.size should be (2)
    }

    it should "deserialize Mention from jsonld" in {
      val json = """
        |{
        |  "@type" : "Extraction",
        |  "@id" : "_:Extraction_1",
        |  "type" : "concept",
        |  "subtype" : "entity",
        |  "labels" : [ "Concept", "Entity" ],
        |  "text" : "Prices",
        |  "rule" : "simple-np++property-lexiconner",
        |  "canonicalName" : "Prices",
        |  "groundings" : [ {
        |    "@type" : "Groundings",
        |    "name" : "wm_compositional/concept",
        |    "category" : "concept",
        |    "version" : "359829afbe9bb5ba9af990121c1bd936a52e7a2e",
        |    "versionDate" : "2019-02-24T01:21:07Z",
        |    "values" : [ {
        |      "@type" : "Grounding",
        |      "ontologyConcept" : "wm_compositional/concept/causal_factor/economic_and_commerce/economic activity/market/revenue",
        |      "value" : 0.5095548033714294
        |    } ]
        |  }, {
        |    "@type" : "Groundings",
        |    "name" : "wm_compositional/property",
        |    "category" : "property"
        |  } ],
        |  "provenance" : [ {
        |    "@type" : "Provenance",
        |    "document" : {
        |      "@id" : "_:Document_1"
        |    },
        |    "documentCharInterval" : [ {
        |      "@type" : "Interval",
        |      "start" : 24323,
        |      "end" : 24328
        |    } ],
        |    "sentence" : {
        |      "@id" : "_:Sentence_481"
        |    },
        |    "sentenceWordPositions" : [ {
        |      "@type" : "Interval",
        |      "start" : 1,
        |      "end" : 1
        |    } ]
        |  } ],
        |
        |  "states" : [ {
        |    "@type" : "State",
        |    "type" : "PROP",
        |    "text" : "Prices"
        |  }, {
        |    "@type" : "State",
        |    "type" : "TIMEX",
        |    "text" : "December 2015.",
        |    "value" : {
        |      "@id" : "_:DCT_1"
        |    }
        |  } ]
        |}""".stripMargin
      val extractionValue = parse(json)
      val documentMap: DocumentMap = Map("_:Document_1" -> null)
      val documentSentenceMap: DocumentSentenceMap = Map("_:Document_1" -> Map("_:Sentence_481" -> 0))
      val extraction = new JLDDeserializer().deserializeExtraction(extractionValue, documentMap, documentSentenceMap)
      val timeIntervel = TimEx(Interval(0, 4), List.empty[TimeStep], "hello there")
      val timexMap = Map("_:DCT_1" -> timeIntervel)
      val geolocMap: GeolocMap = Map.empty
      val mentionMap: MentionMap = Map.empty
      val provenanceMap: ProvenanceMap = Map(Provenance(null, 480, Interval(0, 1)) -> "_:Extraction_1")
      val dctMap: DctMap = Map.empty
      /*val mention =*/ new JLDDeserializer().deserializeMention(extractionValue, extraction, mentionMap,
        documentMap, documentSentenceMap, timexMap, geolocMap, provenanceMap, dctMap)
    }
  }

  def testCorpus(text: String, name: String): Unit = {
    it should "deserialize corpus " + name + " from jsonld" in {
      val oldCorpus = Seq(newTitledAnnotatedDocument(text, name))
      val oldJson = serialize(oldCorpus)

      val newCorpus = new JLDDeserializer().deserialize(oldJson)
      val newJson = serialize(newCorpus)

      val oldLineCount = oldJson.count(_ == '\n')
      val newLineCount = newJson.count(_ == '\n')

      oldLineCount should be (newLineCount)
      oldJson.length should be (newJson.length)
      oldJson should be (newJson)
    }
  }

  def testTextFiles(directoryName: String): Unit = {
    val files = FileUtils.findFiles(directoryName, "txt")

    files.foreach { file =>
      val text = FileUtils.getTextFromFile(file)

      testCorpus(text, file.getName)
    }
  }

  def testJsonldFiles(directoryName: String): Unit = {
    val files = FileUtils.findFiles(directoryName, "jsonld")

    files.foreach { file =>
      val oldText = FileUtils.getTextFromFile(file)
      val oldCorpus = new JLDDeserializer().deserialize(oldText)
      val newText = serialize(oldCorpus)
      val newCorpus = new JLDDeserializer().deserialize(newText)
      val newerText = serialize(newCorpus)

      val oldCanonicalText = oldText.trim.replace("\r", "")
      val newCanonicalText = newText.trim.replace("\r", "")
      val newerCanonicalText = newerText.trim.replace("\r", "")

      if (oldCanonicalText != newCanonicalText)
        println("There was a first problem with " + file.getAbsolutePath)
      if (newCanonicalText != newerCanonicalText)
        println("There was a second problem with " + file.getAbsolutePath)
    }
  }

  def testSentences(): Unit = {
    testCorpus(p1s1, "p1s1")
    testCorpus(p1s2, "p1s2")
    testCorpus(p1s3, "p1s3")
    testCorpus(p1s4, "p1s4")

    testCorpus(p2s1, "p2s1")
    testCorpus(p2s2, "p2s2")
    testCorpus(p2s3, "p2s3")
    testCorpus(p2s4, "p2s4")
    testCorpus(p2s5, "p2s5")

    testCorpus(p3s1, "p3s1")
    testCorpus(p3s2, "p3s2")
    testCorpus(p3s3, "p3s3")
    testCorpus(p3s4, "p3s4")
    testCorpus(p3s5, "p3s5")

    testCorpus(p4s1, "p4s1")
    testCorpus(p4s2, "p4s2")
    testCorpus(p4s3, "p4s3")
    testCorpus(p4s4, "p4s4")

    testCorpus(p5s1, "p5s1")
    testCorpus(p5s2, "p5s2")

    testCorpus(p6s1, "p6s1")
    testCorpus(p6s2, "p6s2")
    testCorpus(p6s3, "p6s3")

    testCorpus("\n", "noSentencesCorpus")
    testCorpus("", "noTextCorpus")
  }

  def testParagraphs(): Unit = {
    testCorpus(p1, "p1")
    testCorpus(p2, "p2")
    testCorpus(p3, "p3")
    testCorpus(p4, "p4")
    testCorpus(p5, "p5")
    testCorpus(p6, "p6")
  }

  def testDocuments(): Unit = {
    testCorpus(fullText, "fullText")
  }

  testParts()
  testSentences()
  testParagraphs()
  testDocuments()

  // Do not run this last test on Travis, but instead periodically on a real corpus
  // with all options enabled (useGrounding, useTimeNorm, useGeoNorm, etc.)
  // To do this with relevance checking, flat grounding must be available!
  // It may need to be changed in ExtractionTest.this to be "eidos" rather than "englishTest".
//  testTextFiles("../corpora/Doc52/txt")
//  testJsonldFiles("../jsonldtmp")
}
