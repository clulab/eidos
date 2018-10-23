[![Build Status](https://travis-ci.org/clulab/eidos.svg?branch=master)](https://travis-ci.org/clulab/eidos)

# Eidos

Eidos is an open-domain machine reading system designed by the [Computational
Language Understanding (CLU) Lab](http://clulab.cs.arizona.edu) at [University
of Arizona](http://www.arizona.edu) for the World Modelers DARPA program.  It
uses a cascade of [Odin](https://github.com/clulab/processors) grammars to
extract events from free text.

Eidos identifies entities like "food insecurity" and a growing list of arguments
on those entities, such as `increases`/`decreases`/`quantifications`.  It
subsequently detects events that occur between entities (directed causal events,
for example) as in "food insecurity causes increased migration".  The list of
arguments and events is updated frequently.  The [JSON LD](https://github.com/clulab/eidos/wiki/JSON-LD)
page documents current output.

**Contents**

- [Usage](#usage)
  - [How to compile the source code](#how-to-compile-the-source-code)
  - [How to run Eidos](#how-to-use-it)
    - [Using the Scala API](#using-the-scala-api)
      - [Prettified display](#to-produce-a-pretty-display-of-the-extracted-mentions)
      - [Export to JSON-LD](#to-export-extractions-as-json-ld)
      - [Export to JSON](#to-serialize-to-json)
    - [Command Line Usage](#command-line-usage)
      - [Interactive shell](#running-an-interactive-shell)
      - [Webapp](#running-the-webapp)
  - [How to use Eidos output](#how-to-use-eidos-output)
    - [Visualizing Eidos output](#visualizing-eidos-output)
    - [Using Eidos output for modeling](#using-eidos-output-for-modeling)
- [License](#license)
- [Related Resources](#related-resources)
- [Notes](#notes)


# Usage

## How to compile the source code

This is a standard [sbt](https://www.scala-sbt.org/) project, so use the usual
commands, e.g., `sbt compile` to compile or `sbt assembly` to create a jar file.
`sbt runMain` can be used to run some of the example applications directly, as
described below.  To access Eidos from Java, add the assembled jar file(s) under
`target/` to your $CLASSPATH.  A file like `eidos-assembly-0.1.6-SNAPSHOT.jar`
may suffice, depending on the build.  If necessary, see `build.sbt` for a list
of runtime dependencies. 

## How to use it

The Eidos system is designed to be used in several ways:

### Using the scala API

The scala API can produce three distinct output formats:
- a pretty display
- a JSON-LD export of the causal graph extracted from the text
- a JSON serialization (in case you want to later load all of the mentions,
  including mentions that are not part of the causal graph)

(see [`src/main/scala/org/clulab/wm/eidos/apps/examples/ExtractFromText.scala`](https://github.com/clulab/eidos/blob/master/src/main/scala/org/clulab/wm/eidos/apps/examples/ExtractFromText.scala))

#### To produce a pretty display of the extracted mentions

```scala
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.displayMention

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  // Display in a pretty way
  annotatedDocument.odinMentions.foreach(displayMention)
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
  val annotatedDocument = reader.extractFromText(text)

  // Export to JSON-LD
  val corpus = new JLDCorpus(Seq(annotatedDocument), reader)
  val mentionsJSONLD = corpus.serialize()
  println(stringify(mentionsJSONLD, pretty = true))
```

This produces the JSON-LD output shown [here](https://github.com/clulab/eidos/wiki/API-output-examples#json-ld-output).

#### To serialize to JSON

```scala
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  // Or... optionally serialize to regular JSON
  // (e.g., if you want to later reload the mentions for post-processing)
  val mentionsJSON = WMJSONSerializer.jsonAST(annotatedDocument.odinMentions)
  println(stringify(mentionsJSON, pretty = true))

```

This produces the JSON serialization [here](https://github.com/clulab/eidos/wiki/API-output-examples#json-output) (mentions may appear in different order):


### Command line usage

#### Extracting causal events from documents in a directory

```bash
sbt "runMain org.clulab.wm.eidos.apps.ExtractFromDirectory /path/to/input/directory /path/to/output/directory"
```

Files in the input directory should end with `txt` and the extracted mentions
from each file will be saved in corresponding JSON-LD files.

**Note**: You cannot use tildes (`~`) in the invocation in lieu of the home
directory.



#### Running an interactive shell

The EidosShell is an interactive shell
for testing the output of Eidos. To run it, do

```bash
./shell
```

#### Running the webapp

To run the webapp version of EidosShell locally, do:

```bash
sbt webapp/run
```

and then navigate to `localhost:9000` in a web browser.


## How to use Eidos output

### Visualizing Eidos output

Eidos reading output can be visualized using
[INDRA](https://github.com/sorgerlab/indra) and Jupyter notebooks. See below for
an example.

![alt text](/doc/delphi_example.png?raw=True")

### Using Eidos output for modeling

Events extracted using Eidos can be converted to
[INDRA](https://github.com/sorgerlab/indra) Influence statements, which are
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
to make probabilistic predictions. 


# License

While we will soon be licensed as Apache, currently one dependency has a GPL
licence.  This will be removed very soon and the license will be updated.



# Related resources

If you are working on this project, you may be interested in [additional
materials](https://drive.google.com/open?id=1cHJIfQTr0XE2CEqbo4POSm0-_xzrDP-A)
stored in the cloud. Access may be limited by permission settings.  Other
documents are included in the /doc directory of the repository.

Of particular interest for those involved with grounding are two very large files
of vectors that are used for the Word2Vec algorithm.  They are

- [vectors.txt](https://drive.google.com/open?id=1tffQuLB5XtKcq9wlo0n-tsnPJYQF18oS) and
- [glove.840B.300d.txt.tgz](https://drive.google.com/open?id=1k4Bc3iNWId8ac_fmkr9yFKhQEycdwOVk).

To use these files, download one or both and place them in the project's
`src/main/resources/org/clulab/wm/eidos/w2v` directory.  The first file can be used as is, but
the second file needs to be extracted before use with a command like
`tar xvzf glove.840B.300d.txt.tgz`.)  Indicate to Eidos
that they should be used by setting `useW2V = true` in
`src/main/resources/eidos.conf`.  Only one can be used at a time, so make sure the
appropriate one of these lines is uncommented, without the `//`.

- `wordToVecPath = "/org/clulab/wm/eidos/w2v/vectors.txt"`
- `wordToVecPath = "/org/clulab/wm/eidos/w2v/glove.840B.300d.vectors.txt"`

Processing the files can consume multiple minutes of time, so if you want to run
Eidos more than a couple of times with the vectors, then it's useful to cache a
processed version of them along with the ontologies they work with.  This can be
accomplished with the command
```
sbt "runMain org.clulab.wm.eidos.apps.CacheOntologies"
```
This will by default write serialized versions of the known ontologies and configured
vector file to `./cache/`.  To use the cached copies, set `useCachedOntologies = true`
in `src/main/resources/eidos.conf`.



# Notes

The default size of the memory allocation pool for the JVM is 1/4 of your
physical memory, but Eidos may require
more RAM than that.  It is currently being developed and tested with a 6GB limit.

For those using `sbt`, the file `.jvmopts` is included with the source code to
arrange for more memory.  No other changes should be necessary.  Alternatively,
an environment variable specific to `sbt` can be used:

`SBT_OPTS=-Xmx6g`

IDEs and other development tools are generally unaware of `.jvmopts`, but can be
configured via an environment variable instead.

`JAVA_TOOL_OPTIONS=-Xmx6g`

Other situations may require a more general setting.

`_JAVA_OPTIONS=-Xmx6g`

The procedure for defining these variables is dependent on operating system and
shell.
