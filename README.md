[![Build Status](https://travis-ci.org/clulab/eidos.svg?branch=master)](https://travis-ci.org/clulab/eidos)

# Eidos

Eidos is an open-domain machine reading system designed by the [Computational Language Understanding (CLU) Lab](http://clulab.cs.arizona.edu) at [University of Arizona](http://www.arizona.edu) for the World Modelers DARPA program.  Eidos uses a cascade of [Odin](https://github.com/clulab/processors) grammars to extract causal events from free text.

Currently we extract entities such as "food insecurity" (and `increases`/`decreases`/`quantifications` of those entities) and directed causal events that occur between entities such as "food insecurity causes increased migration".  In the near future we plan to expand this to also extract correlation, `same-as`, and `is-a` relations. 

# Usage

## How to compile the source code

This is a standard [sbt](https://www.scala-sbt.org/) project, so use the usual commands, e.g., `sbt compile` to compile
or `sbt assembly` to create a jar file.  `sbt runMain` can be used to run some of the example applications directly, as
described below.  To access Eidos from Java, add the assembled jar file(s) under `target/` to your $CLASSPATH.  A file
like `eidos-assembly-0.1.6-SNAPSHOT.jar` may suffice, depending on the build.  If necessary, see `build.sbt` for a
list of runtime dependencies. 

## How to use it

The Eidos system is designed to be used in several ways:

### Using the scala API

The scala API can produce three distinct output formats:
- a pretty display
- a JSON-LD export of the causal graph extracted from the text
- a JSON serialization (in case you want to later load all of the mentions, including mentions that are not part of the causal graph)

(see [`src/main/scala/org/clulab/wm/eidos/apps/examples/ExtractFromText.scala`](https://github.com/clulab/eidos/blob/master/src/main/scala/org/clulab/wm/eidos/apps/examples/ExtractFromText.scala))

#### To produce a pretty display of the extracted mentions

```scala
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.displayMention

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFrom(text)

  // Display in a pretty way
  annotatedDocument.mentions.foreach(displayMention)
```

This produces the following output (mentions may appear in different order):
```
List(NounPhrase, Entity) => Water trucking
	------------------------------
	Rule => simple-np++Decrease_ported_syntax_2_verb
	Type => TextBoundMention
	------------------------------
	NounPhrase, Entity => Water trucking
	  * Attachments: Decrease(decreased,None)
	------------------------------

List(NounPhrase, Entity) => cost of fuel
	------------------------------
	Rule => simple-np
	Type => TextBoundMention
	------------------------------
	NounPhrase, Entity => cost of fuel
	------------------------------

List(Causal, DirectedRelation, EntityLinker, Event) => Water trucking has decreased due to the cost of fuel
	------------------------------
	Rule => dueToSyntax2-Causal
	Type => EventMention
	------------------------------
	trigger => due
	cause (NounPhrase, Entity) => cost of fuel
	effect (NounPhrase, Entity) => Water trucking
	  * Attachments: Decrease(decreased,None)
	------------------------------
```

#### To export extractions as JSON-LD

(For information about the JSON-LD export format, please look [here](https://github.com/clulab/eidos/wiki/JSON-LD).)


```scala
import scala.collection.Seq
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFrom(text)

  // Export to JSON-LD
  val corpus = new JLDCorpus(Seq(annotatedDocument), reader)
  val mentionsJSONLD = corpus.serialize()
  println(stringify(mentionsJSONLD, pretty = true))
```

This produces the following JSON-LD output (mentions may appear in different order):
```
{
  "@context" : {
    "@base" : "https://github.com/clulab/eidos/wiki/JSON-LD",
    "Corpus" : "#Corpus",
    "Dependency" : "#Dependency",
    "DirectedRelation" : "#DirectedRelation",
    "Document" : "#Document",
    "Entity" : "#Entity",
    "Interval" : "#Interval",
    "Modifier" : "#Modifier",
    "Provenance" : "#Provenance",
    "Sentence" : "#Sentence",
    "State" : "#State",
    "Trigger" : "#Trigger",
    "Word" : "#Word"
  },
  "@type" : "Corpus",
  "documents" : [ {
    "@type" : "Document",
    "@id" : "_:Document_1",
    "sentences" : [ {
      "@type" : "Sentence",
      "@id" : "_:Sentence_1",
      "text" : "The significant increase in rainfall has caused worsened crop yields .",
      "words" : [ {
        "@type" : "Word",
        "@id" : "_:Word_1",
        "tag" : "DT",
        "entity" : "O",
        "startOffset" : 0,
        "endOffset" : 3,
        "lemma" : "the",
        "chunk" : "B-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_2",
        "tag" : "JJ",
        "entity" : "B-Quantifier",
        "startOffset" : 4,
        "endOffset" : 15,
        "lemma" : "significant",
        "chunk" : "I-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_3",
        "tag" : "NN",
        "entity" : "O",
        "startOffset" : 16,
        "endOffset" : 24,
        "lemma" : "increase",
        "chunk" : "I-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_4",
        "tag" : "IN",
        "entity" : "O",
        "startOffset" : 25,
        "endOffset" : 27,
        "lemma" : "in",
        "chunk" : "B-PP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_5",
        "tag" : "NNS",
        "entity" : "O",
        "startOffset" : 28,
        "endOffset" : 36,
        "lemma" : "rainfall",
        "chunk" : "B-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_6",
        "tag" : "VBZ",
        "entity" : "O",
        "startOffset" : 37,
        "endOffset" : 40,
        "lemma" : "have",
        "chunk" : "B-VP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_7",
        "tag" : "VBN",
        "entity" : "O",
        "startOffset" : 41,
        "endOffset" : 47,
        "lemma" : "cause",
        "chunk" : "I-VP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_8",
        "tag" : "VBN",
        "entity" : "O",
        "startOffset" : 48,
        "endOffset" : 56,
        "lemma" : "worsen",
        "chunk" : "I-VP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_9",
        "tag" : "NN",
        "entity" : "O",
        "startOffset" : 57,
        "endOffset" : 61,
        "lemma" : "crop",
        "chunk" : "B-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_10",
        "tag" : "NNS",
        "entity" : "O",
        "startOffset" : 62,
        "endOffset" : 68,
        "lemma" : "yield",
        "chunk" : "I-NP"
      }, {
        "@type" : "Word",
        "@id" : "_:Word_11",
        "tag" : ".",
        "entity" : "O",
        "startOffset" : 68,
        "endOffset" : 69,
        "lemma" : ".",
        "chunk" : "O"
      } ],
      "dependencies" : [ {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_3"
        },
        "destination" : {
          "@id" : "_:Word_1"
        },
        "relation" : "det"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_3"
        },
        "destination" : {
          "@id" : "_:Word_2"
        },
        "relation" : "amod"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_3"
        },
        "destination" : {
          "@id" : "_:Word_5"
        },
        "relation" : "nmod_in"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_5"
        },
        "destination" : {
          "@id" : "_:Word_4"
        },
        "relation" : "case"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_7"
        },
        "destination" : {
          "@id" : "_:Word_3"
        },
        "relation" : "nsubj"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_7"
        },
        "destination" : {
          "@id" : "_:Word_6"
        },
        "relation" : "aux"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_7"
        },
        "destination" : {
          "@id" : "_:Word_10"
        },
        "relation" : "dobj"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_7"
        },
        "destination" : {
          "@id" : "_:Word_11"
        },
        "relation" : "punct"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_10"
        },
        "destination" : {
          "@id" : "_:Word_8"
        },
        "relation" : "amod"
      }, {
        "@type" : "Dependency",
        "source" : {
          "@id" : "_:Word_10"
        },
        "destination" : {
          "@id" : "_:Word_9"
        },
        "relation" : "compound"
      } ]
    } ]
  } ],
  "extractions" : [ {
    "@type" : "Entity",
    "@id" : "_:Entity_1",
    "labels" : [ "NounPhrase", "Entity" ],
    "text" : "crop yields",
    "rule" : "simple-np++Decrease_ported_syntax_6_verb",
    "provenance" : [ {
      "@type" : "Provenance",
      "document" : {
        "@id" : "_:Document_1"
      },
      "sentence" : {
        "@id" : "_:Sentence_1"
      },
      "positions" : {
        "@type" : "Interval",
        "start" : 9,
        "end" : 10
      }
    } ],
    "states" : [ {
      "@type" : "State",
      "type" : "DEC",
      "text" : "worsened"
    } ]
  }, {
    "@type" : "Entity",
    "@id" : "_:Entity_2",
    "labels" : [ "NounPhrase", "Entity" ],
    "text" : "rainfall",
    "rule" : "simple-np++Increase_ported_syntax_1_noun",
    "provenance" : [ {
      "@type" : "Provenance",
      "document" : {
        "@id" : "_:Document_1"
      },
      "sentence" : {
        "@id" : "_:Sentence_1"
      },
      "positions" : {
        "@type" : "Interval",
        "start" : 5,
        "end" : 5
      }
    } ],
    "states" : [ {
      "@type" : "State",
      "type" : "INC",
      "text" : "increase",
      "modifiers" : [ {
        "@type" : "Modifier",
        "text" : "significant",
        "intercept" : 0.6154,
        "mu" : 1.034E-5,
        "sigma" : -0.001123
      } ]
    } ]
  }, {
    "@type" : "DirectedRelation",
    "@id" : "_:DirectedRelation_1",
    "labels" : [ "Causal", "DirectedRelation", "EntityLinker", "Event" ],
    "text" : "rainfall has caused worsened crop yields",
    "rule" : "ported_syntax_1_verb-Causal",
    "provenance" : [ {
      "@type" : "Provenance",
      "document" : {
        "@id" : "_:Document_1"
      },
      "sentence" : {
        "@id" : "_:Sentence_1"
      },
      "positions" : {
        "@type" : "Interval",
        "start" : 5,
        "end" : 10
      }
    } ],
    "trigger" : {
      "@type" : "Trigger",
      "text" : "caused",
      "provenance" : [ {
        "@type" : "Provenance",
        "document" : {
          "@id" : "_:Document_1"
        },
        "sentence" : {
          "@id" : "_:Sentence_1"
        },
        "positions" : {
          "@type" : "Interval",
          "start" : 7,
          "end" : 7
        }
      } ]
    },
    "sources" : [ {
      "@id" : "_:Entity_2"
    } ],
    "destinations" : [ {
      "@id" : "_:Entity_1"
    } ]
  } ]
}
```

#### To serialize to JSON

```scala
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFrom(text)

  // Or... optionally serialize to regular JSON
  // (e.g., if you want to later reload the mentions for post-processing)
  val mentionsJSON = WMJSONSerializer.jsonAST(annotatedDocument.mentions)
  println(stringify(mentionsJSON, pretty = true))

```

This produces the following JSON serialization (mentions may appear in different order):

```
{
  "documents" : {
    "-1501028087" : {
      "sentences" : [ {
        "words" : [ "Water", "trucking", "has", "decreased", "due", "to", "the", "cost", "of", "fuel", "." ],
        "startOffsets" : [ 0, 6, 15, 19, 29, 33, 36, 40, 45, 48, 52 ],
        "endOffsets" : [ 5, 14, 18, 28, 32, 35, 39, 44, 47, 52, 53 ],
        "tags" : [ "NN", "NN", "VBZ", "VBN", "JJ", "TO", "DT", "NN", "IN", "NN", "." ],
        "lemmas" : [ "water", "trucking", "have", "decrease", "due", "to", "the", "cost", "of", "fuel", "." ],
        "entities" : [ "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O" ],
        "norms" : [ "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O" ],
        "chunks" : [ "B-NP", "I-NP", "B-VP", "I-VP", "B-ADJP", "B-PP", "B-NP", "I-NP", "B-PP", "B-NP", "O" ],
        "graphs" : {
          "universal-enhanced" : {
            "edges" : [ {
              "source" : 1,
              "destination" : 0,
              "relation" : "compound"
            }, {
              "source" : 3,
              "destination" : 1,
              "relation" : "nsubj"
            }, {
              "source" : 3,
              "destination" : 2,
              "relation" : "aux"
            }, {
              "source" : 3,
              "destination" : 7,
              "relation" : "nmod_due_to"
            }, {
              "source" : 3,
              "destination" : 10,
              "relation" : "punct"
            }, {
              "source" : 4,
              "destination" : 5,
              "relation" : "mwe"
            }, {
              "source" : 7,
              "destination" : 4,
              "relation" : "case"
            }, {
              "source" : 7,
              "destination" : 6,
              "relation" : "det"
            }, {
              "source" : 7,
              "destination" : 9,
              "relation" : "nmod_of"
            }, {
              "source" : 9,
              "destination" : 8,
              "relation" : "case"
            } ],
            "roots" : [ 3 ]
          },
          "universal-basic" : {
            "edges" : [ {
              "source" : 1,
              "destination" : 0,
              "relation" : "compound"
            }, {
              "source" : 3,
              "destination" : 1,
              "relation" : "nsubj"
            }, {
              "source" : 3,
              "destination" : 2,
              "relation" : "aux"
            }, {
              "source" : 3,
              "destination" : 7,
              "relation" : "nmod"
            }, {
              "source" : 3,
              "destination" : 10,
              "relation" : "punct"
            }, {
              "source" : 4,
              "destination" : 5,
              "relation" : "mwe"
            }, {
              "source" : 7,
              "destination" : 4,
              "relation" : "case"
            }, {
              "source" : 7,
              "destination" : 6,
              "relation" : "det"
            }, {
              "source" : 7,
              "destination" : 9,
              "relation" : "nmod"
            }, {
              "source" : 9,
              "destination" : 8,
              "relation" : "case"
            } ],
            "roots" : [ 3 ]
          }
        }
      } ]
    }
  },
  "mentions" : [ {
    "type" : "TextBoundMention",
    "id" : "T:-1364009445",
    "text" : "Water trucking",
    "labels" : [ "NounPhrase", "Entity" ],
    "tokenInterval" : {
      "start" : 0,
      "end" : 2
    },
    "characterStartOffset" : 0,
    "characterEndOffset" : 14,
    "sentence" : 0,
    "document" : "-1501028087",
    "keep" : true,
    "foundBy" : "simple-np++Decrease_ported_syntax_2_verb",
    "attachments" : [ {
      "type" : "Decrease",
      "mod" : "{\"trigger\":\"decreased\"}"
    } ]
  }, {
    "type" : "TextBoundMention",
    "id" : "T:-737414104",
    "text" : "cost of fuel",
    "labels" : [ "NounPhrase", "Entity" ],
    "tokenInterval" : {
      "start" : 7,
      "end" : 10
    },
    "characterStartOffset" : 40,
    "characterEndOffset" : 52,
    "sentence" : 0,
    "document" : "-1501028087",
    "keep" : true,
    "foundBy" : "simple-np",
    "attachments" : [ ]
  }, {
    "type" : "EventMention",
    "id" : "E:-1344481171",
    "text" : "Water trucking has decreased due to the cost of fuel",
    "labels" : [ "Causal", "DirectedRelation", "EntityLinker", "Event" ],
    "trigger" : {
      "type" : "TextBoundMention",
      "id" : "T:87479703",
      "text" : "due",
      "labels" : [ "Causal", "DirectedRelation", "EntityLinker", "Event" ],
      "tokenInterval" : {
        "start" : 4,
        "end" : 5
      },
      "characterStartOffset" : 29,
      "characterEndOffset" : 32,
      "sentence" : 0,
      "document" : "-1501028087",
      "keep" : true,
      "foundBy" : "dueToSyntax2-Causal",
      "attachments" : [ ]
    },
    "arguments" : {
      "cause" : [ {
        "type" : "TextBoundMention",
        "id" : "T:-737414104",
        "text" : "cost of fuel",
        "labels" : [ "NounPhrase", "Entity" ],
        "tokenInterval" : {
          "start" : 7,
          "end" : 10
        },
        "characterStartOffset" : 40,
        "characterEndOffset" : 52,
        "sentence" : 0,
        "document" : "-1501028087",
        "keep" : true,
        "foundBy" : "simple-np",
        "attachments" : [ ]
      } ],
      "effect" : [ {
        "type" : "TextBoundMention",
        "id" : "T:-1364009445",
        "text" : "Water trucking",
        "labels" : [ "NounPhrase", "Entity" ],
        "tokenInterval" : {
          "start" : 0,
          "end" : 2
        },
        "characterStartOffset" : 0,
        "characterEndOffset" : 14,
        "sentence" : 0,
        "document" : "-1501028087",
        "keep" : true,
        "foundBy" : "simple-np++Decrease_ported_syntax_2_verb",
        "attachments" : [ {
          "type" : "Decrease",
          "mod" : "{\"trigger\":\"decreased\"}"
        } ]
      } ]
    },
    "paths" : {
      "cause" : {
        "T:-737414104" : [ {
          "source" : 7,
          "destination" : 4,
          "relation" : "case"
        } ]
      },
      "effect" : {
        "T:-1364009445" : [ {
          "source" : 7,
          "destination" : 4,
          "relation" : "case"
        }, {
          "source" : 3,
          "destination" : 7,
          "relation" : "nmod_due_to"
        }, {
          "source" : 3,
          "destination" : 1,
          "relation" : "nsubj"
        } ]
      }
    },
    "tokenInterval" : {
      "start" : 0,
      "end" : 10
    },
    "characterStartOffset" : 0,
    "characterEndOffset" : 52,
    "sentence" : 0,
    "document" : "-1501028087",
    "keep" : true,
    "foundBy" : "dueToSyntax2-Causal",
    "attachments" : [ ]
  } ]
}
```

### Extracting causal events from documents in a directory

```bash
sbt "runMain org.clulab.wm.ExtractFromDirectory /path/to/input/directory /path/to/output/directory"
```
Files in the input directory should end with `txt` and the extracted mentions from each file will be saved in corresponding JSON-LD files.



### Running an interactive shell

The EidosShell is an interactive shell
for testing the output of Eidos. To run it, do

```
./shell
```

To run the webapp version of EidosShell locally, do:

```bash
sbt webapp/run
```

and then navigate to `localhost:9000` in a web browser.



### Using Eidos output for modeling

Events extracted using Eidos can be converted to
[INDRA-GEM](https://github.com/sorgerlab/indra) Influence statements, which are
bespoke data structures designed for modeling causal networks. 

Example usage:

```python
>>> from indra.sources import eidos
>>> ep = eidos.process_text("Water trucking has decreased due to the cost of fuel.")
>>> ep.statements
[Influence(cost of fuel(), Water trucking(negative))]
```

[Delphi](https://github.com/ml4ai/delphi) is a framework built on top of INDRA
that assembles causal fragments extracted by Eidos into a causal analysis graph.
This causal analysis graph is then converted to a dynamic Bayes network and used
to make probabilistic predictions. Currently Delphi provides a webapp interface
for interactive visualization of CAGs. Here is an example:

![alt text](/doc/delphi_example.png?raw=True")


## License

While we will soon be licensed as Apache, currently one dependency has a GPL licence.  This will be removed very soon and the license will be updated.



## Related resources

If you are working on this project, you may be interested in [additional materials](https://drive.google.com/open?id=1cHJIfQTr0XE2CEqbo4POSm0-_xzrDP-A) stored in the cloud. Access may be limited by permission settings.  Other documents are included in the /doc directory of the repository.

There is one [large file of vectors](https://drive.google.com/open?id=1tffQuLB5XtKcq9wlo0n-tsnPJYQF18oS) which is useful at runtime if you are interested in ontological grounding.  To use this file, download it and place it in the project's `src/main/resources/org/clulab/wm/eidos/w2v` directory.  Then indicate to Eidos that it should be used by setting `useW2V = true` in `src/main/resources/eidos.conf`.


## Notes

The default size of the memory allocation pool for the JVM is 1/4 of your
physical memory, but Eidos may require more RAM than that.  For some operating
systems (apparently not Windows), you can increase the
memory allocation by specifying it in the `.sbtopts` file in the `eidos`
directory (the one in which this README resides): 

```bash
echo "-J-Xmx6g" >> .sbtopts
```

The flag enclosed in the quotes allocates 6 GB, which should be sufficient.

For Windows, it may be necessary to set an environment variable that influences how
much memory is allowed for Java in general.  On the traditional Command Prompt use

```
set JAVA_OPTS=-Xmx6g
```

The corresponding command for PowerShell is

```
$env:JAVA_OPTS = "-Xmx6g"
```
