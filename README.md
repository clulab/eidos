
# Eidos

Eidos is an open-domain machine reading system designed by the [Computational
Language Understanding (CLU) Lab](http://clulab.cs.arizona.edu) at [University
of Arizona](http://www.arizona.edu) for the World Modelers DARPA program and
then further refined and extended by [LUM.ai](https://lum.ai/) for professional
and commercial application.  It uses a cascade of [Odin](https://github.com/clulab/processors)
grammars to extract events from free text.

<hr>

# CauseEx

Eidos for CauseEx (Causal Extraction) has all the capabilities of the version for [World Modelers](#world-modelers)
described below and more.  Although it is very important to understand the base functionality provided by the World Modelers
version, this is the repository for the CauseEx version, so in this top section, only the additions are highlighted.

## Kafka Interface

Eidos can be incorportated into [Kafka streams](https://kafka.apache.org/24/documentation/streams/)
and used in a pipeline architecture.  Example Eidos producers, streams, and consumers are included in
the [kafka](https://github.com/lum-ai/eidos/tree/master/kafka) subproject.  The code below, for example,
is used as part of a pipeline in which a producer inputs json files containing document text and metadata,
the stream component reads the document, converting it from json to jsonld, and the consumer outputs the
resulting jsonld files.
```scala
package ai.lum.eidos.kafka.stream
...
class EidosStream(inputTopic: String, outputTopic: String, properties: Properties, eidosSystem: EidosSystem,
    options: EidosSystem.Options, adjectiveGrounder: EidosAdjectiveGrounder) {

  protected def process(cdr: String): String = {
    val cdrText = CdrText(cdr)
    val annotatedDocument = eidosSystem.extractFromText(cdrText.getText, options, cdrText.getMetadata)
    val jsonld = stringify(new JLDCorpus(annotatedDocument).serialize(adjectiveGrounder), pretty = true)

    jsonld
  }

  protected val stream: KafkaStreams = {
    val topology = TopologyBuilder.fromStreamsBuilder { streamsBuilder =>
      streamsBuilder
          .stream[String, String](inputTopic)
          .map { (key: String, cdr: String) =>
            println("Streaming " + key)
            key -> process(cdr)
          }
          .to(outputTopic)
    }.get

    new KafkaStreams(topology, properties)
  }
  ...
}
```
This code assumes that Kafka and Zookeeper are already running and configured in a way compatible
with the properties files consulted by the producer, streams, and consumer apps which in turn
create the Eidos producer, streams, and consumer objects.  For more information, please consult
Kafka documentation, particularly that from [Apache](https://kafka.apache.org/documentation/) and
[Confluent](https://docs.confluent.io/current/getting-started.html).


## REST Interface


## Metadata

It is more and more the case that metadata is provided along with document text.  `EidosSystem` still supports
its traditional operations on plain text, but several entry points have been added that accept metadata.
Metadata currently includes document title, id, location (i.e., where it is stored), and DCT (document creation time).
It is usually provided via an object extending the `EidosText` trait.  This is a simple interface which provides
`getText` and `getMetadata` methods.  Implementations include `CdrText`, `CluText`, and `PlainText`.  Where one
might previously write
```scala
val text = FileUtils.getTextFromFile(file)
val annotatedDocument = eidosSystem.extractFromText(text)
```
one can now upgrade to
```scala
val cdrText = CdrText(file)
val annotatedDocument = eidosSystem.extractFromText(cdrText.getText, EidosSystem.Options, cdrText.getMetadata)
```
in order to make use of the metadata.

## Command Line

Eidos can be accessed from the command line in numerous ways.  There are over 40 Java-style command line
applications included in the source code.  These are most easily accessed via `sbt`, but Eidos can
also be assembled (run `sbt assembly`) so that a single jar file is produced for more direct access with
Java.  In the past, the `ExtractFromDirectory` program was often used, but with the addition of metadata,
`ExtractCdrMetaFromDirectory` is more functional.  Its parameters are input directory, output directory,
time file, and thread count so that a typical call is
```sh
$ sbt "runMain org.clulab.wm.eidos.apps.batch.ExtractCdrMetaFromDirectory ../json ../jsonld time.txt 8"
```

## Ontologies

An additional ontology can be configured, but presently its files must be deployed manually to the local machine
before it can be used.  It is configured by editing the file `causeex.conf` and changing the value of `ontologies`
to include `"two_six"` in the list that may still include other active ontologies as such:
```
ontologies = ["two_six", "wm_flattened", others...]
```

<hr>

# World Modelers

[![Build Status](http://jenkins.cs.arizona.edu:8090/buildStatus/icon?job=eidos%2Fmaster)](http://jenkins.cs.arizona.edu:8090/job/eidos)

Eidos for World Modelers identifies entities like "food insecurity" along with a growing list of arguments
on those entities, such as `increases` / `decreases` / `quantifications`.  It
subsequently detects events that occur between entities (directed causal events,
for example) as in "food insecurity causes increased migration".  The list of
arguments and events is updated frequently and is documented on the [JSON-LD](https://github.com/clulab/eidos/wiki/JSON-LD)
page.


## Contents

- Running Eidos
  - Using the Sample Apps
    - [Webapp](#webapp)
    - [Web Service](#web-service)
    - [Interactive Shell](#interactive-shell)
    - [ExtractAndExport](#extractandexport)
    - [Other Apps](#other-apps)
  - Using the Scala API
    - [Prettified Display](#prettified-display)
    - [Export to JSON-LD](#export-to-json-ld)
- Connecting Eidos
  - [INDRA](#indra)
  - [Delphi](#delphi)
- Working with Eidos
  - [Compiling](#compiling)
  - [Configuring](#configuring)
  - [Optimizing](#optimizing)
  - [Translating](#translating)
  - [Integrating](#integrating)
- [Notes](#notes)
- [License](#license)


# Running Eidos

Eidos in this repository is formatted as a standard [sbt](https://www.scala-sbt.org/) project,
that's the Scala Build Tool, so you will need to download and install `sbt` to "run" the
included programs as described here.  (See the section on [integrating](#integrating) for other
ways to use Eidos.)  `sbt` in turn requires Java, so that will need to have
been installed as well, but that's the extent of it.  Other software packages that Eidos depends
on will be downloaded automatically by `sbt`.  The contents of this repository can be downloaded
with `git` or via the "Clone or download" button on the project's GitHub page.  Assuming you
have done this and any necessary unzipping, you should have a directory called `eidos` containing
the file `build.sbt` among others.  `cd` to this directory.


## Using the Sample Apps

The software includes several sample applications that demonstrate its abilities.  Some are more
colorful and interactive and others are tailored for machine readable output.  They are fairly
large programs and the software is configured in file `.jvmopts` to supply Java with lots of memory.
If you see complaints about inadequate heap space, please check the [notes](#notes) section for
possible remedies.


### Webapp

The webapp provides the most colorful, graphical, and perhaps easiest to understand output.
It can be started up directly from the command line in one fell swoop
```bash
> sbt webapp/run
```
or from within `sbt` with
```bash
> sbt
sbt:eidos> webapp/run
```

`sbt` may take several minutes to bring up the application, especially the first time, as various
dependencies are downloaded and Eidos itself is compiled.  Numerous logging messages should keep
you posted on the progress.

After starting the webapp, use a web browser to navigate to `localhost:9000`.  There you should
see something like this:

![Webapp window without text](/doc/webapp_empty.png?raw=True")

You can now submit texts for Eidos to process.  Please note that the very first submission will
require extra time as lazily loaded parts of the system are initialized, but subsequent texts
will be processed much more quickly.

![Webapp window with text](/doc/webapp_full.png?raw=True")

To eventually escape from `sbt`, you can stop the web server with Control-D and then quit the
program with `exit`.

### Web Service
When the webapp is run, it exposes a web service at port `9000` via the `/process_text` endpoint. It accepts a `POST` request and requires JSON with the following parameter:

* `text`: the text you wish to submit for parsing by Eidos.

For example, we can use the Python `requests` library to interact with the web service with the following:

```python
import requests

text = """Drought increases regional insecurity."""

webservice = 'http://localhost:9000'
res = requests.post('%s/process_text' %webservice, headers={'Content-type': 'application/json'}, json={'text': text})

json_dict = res.json()
```

Using `CURL` we can do the same with:

```bash
> curl \
  --header "Content-type: application/json" \
  --request POST \
  --data '{"text": "Drought increases regional insecurity."}' \
  http://localhost:9000/process_text
```

### Interactive Shell

EidosShell is an interactive text-based program for testing the output of Eidos.  Its output is
less complete than the webapp, but it stands alone, no web browser is required, and it consumes
fewer resources.  A script is provided to run it with
```bash
> ./shell
```
or
```DOS.bat
> .\shell
```
depending on operating system, or it can be run in the usual `sbt` way with
```bash
> sbt "runMain org.clulab.wm.eidos.apps.EidosShell"
```
or
```bash
> sbt
eidos:sbt> runMain org.clulab.wm.eidos.apps.EidosShell
```

You should eventually be prompted for an input text or command:
```
(Eidos)>>> Food insecurity causes increased migration.
```

The first text processed will again take extra time as the lazily loaded parts are initialized,
but the eventual pertinent output should look substantially similar to
```
sentence #0
Food insecurity causes increased migration .
Tokens: (0,Food,NNP), (1,insecurity,NN), (2,causes,VBZ), (3,increased,VBN), (4,migration,NN), (5,.,.)
roots: 2
outgoing:
        0:
        1: (0,compound)
        2: (1,nsubj) (4,dobj) (5,punct)
        3:
        4: (3,amod)
        5:
incoming:
        0: (1,compound)
        1: (2,nsubj)
        2:
        3: (4,amod)
        4: (2,dobj)
        5: (2,punct)


timeExpressions:

entities:
List(Concept, Entity) => Food insecurity
         ------------------------------
         Rule => simple-np
         Type => TextBoundMention
         ------------------------------
         Concept, Entity => Food insecurity
         ------------------------------


List(Concept, Entity) => increased migration
         ------------------------------
         Rule => simple-np++Increase_ported_syntax_6_verb
         Type => TextBoundMention
         ------------------------------
         Concept, Entity => increased migration
          * Attachments: Increase(increased,None)
         ------------------------------



events:
List(Causal, DirectedRelation, EntityLinker, Event) => Food insecurity causes increased migration
         ------------------------------
         Rule => ported_syntax_1_verb-Causal
         Type => EventMention
         ------------------------------
         trigger => causes
         cause (Concept, Entity) => Food insecurity
         effect (Concept, Entity) => increased migration
          * Attachments: Increase(increased,None)

         ------------------------------


==================================================
```

To exit the program, enter `:exit` as the menu indicates.  Another `exit` will close `sbt`.


### ExtractAndExport

If the texts to be processed can be placed into one or more files, especially if the files
are located in the current directory and end with `.txt`, then the `ExtractAndExport` app is
particularly handy.  The expected location of the files, the extension, and several
other parameters are actually specified in a configuration file, `reference.conf`, which
can be edited.  (Other apps from the section below allow command line specification of
more of the parameters.)  However, if default values are satisfactory, one command will process
the *.txt files into *.txt.jsonld, *.txt.mitre.tsv, and *.txt.serialized with output in
corresponding formats.
```bash
> sbt "runMain org.clulab.wm.eidos.apps.ExtractAndExport"
```
or of course
```bash
> sbt
std:eidos> runMain org.clulab.wm.eidos.apps.ExtractAndExport
```


### Other Apps

Other apps are located in directory `src/main/scala/org/clulab/wm/eidos/apps`.
`ExtractFromDirectory`, for instance, is similar to the previously described app,
but it allows specification of directories on the command line and only outputs
JSON-LD format.  The command is

```bash
> sbt "runMain org.clulab.wm.eidos.apps.ExtractFromDirectory path/to/input/directory path/to/output/directory"
```

or its two-line counterpart.  Again, files in the input directory should end with `.txt` and
the extracted mentions from each file will be saved in corresponding JSON-LD files.
**Note** that you cannot use tildes (`~`) in the invocation in lieu of the home directory for
this and most Java-like programs.


## Using the Scala API

One is of course not limited to using the existing apps: one can write new programs which
make use of Eidos's Scala/Java interface.  New programs can be contained in the same `sbt` project
or a different one, in Java projects, in Python projects, or elsewhere.  See the
section on [integration](#integration) for tips on how this is accomplished.
This description assumes that a program is added to the collection of existing apps in
their `apps` directory or somewhere nearby.  There are two major output formats provided
through the Scala API.


### Prettified Display

To produce a pretty display of the extracted mentions, use `EidosSystem` to extract
an `annotatedDocument` from the text and then display its mentions.
```scala
package org.yourself.eidosClient

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.displayMention

object YourClassName extends App {
  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  // Display in a pretty way
  annotatedDocument.odinMentions.foreach(displayMention)
}
```

When called with
```bash
sbt:eidos> runMain org.yourself.eidosClient.YourClassName
```
it should produce output similar to this:

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

This description is based on the [ExtractFromText](https://github.com/clulab/eidos/blob/master/src/main/scala/org/clulab/wm/eidos/apps/examples/ExtractFromText.scala) app.


### Export to JSON-LD

To export extractions, and in fact causal graphs, as JSON-LD, take the `annotatedDocument`, create a `JLDCorpus`,
serialize it, and then print a pretty string version.
```scala
package org.yourself.eidosClient

import scala.collection.Seq
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus

object YourClassName extends App {
  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  // Export to JSON-LD
  val corpus = new JLDCorpus(Seq(annotatedDocument), reader.loadableAttributes.adjectiveGrounder)
  val mentionsJSONLD = corpus.serialize()
  println(stringify(mentionsJSONLD, pretty = true))
}
```

The output that results when such code is called with

```bash
sbt:eidos> runMain org.yourself.eidosClient.YourClassName
```

can be [quite long](https://github.com/clulab/eidos/wiki/API-output-examples#json-ld-output).
The [JSON-LD export format](https://github.com/clulab/eidos/wiki/JSON-LD) for Eidos is defined
on a wiki page.


# Connecting Eidos

Output of Eidos can already be connected to other tools for visualization and modelling.
Follow their links for details.


## INDRA

Eidos reading output (JSON-LD) can be visualized using [INDRA](https://github.com/sorgerlab/indra)
and Jupyter notebooks. Here is an example:
![Link to Delphi](/doc/indra_example.png?raw=True")


## Delphi

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


# Working with Eidos


## Compiling

This is a standard [sbt](https://www.scala-sbt.org/) project, so the usual
command, `sbt compile`, can be used for compilation.  A plugin is included which
can be used to assemble a fat jar file with the command `sbt assembly`.
Each of these commands results in files being written to the directory `target`.
`sbt runMain` can run files that are (temporarily) stored in that location.

The project can be readily imported into IntelliJ IDEA with the Scala plugin
and compiled, run, and debugged from there.  In addition, the Eclipse plugin
is configured for use.  The project can be converted for use with a Scala-fortified
Eclipse with the command
```bash
> sbt eclipse
```


## Configuring

There are two main configuration files for Eidos: `reference.conf` and `eidos.conf`
in the directory `src/main/resources`.  The former is meant to contain default values
that aren't very likely to change.  The latter is where users are advised to make
adjustments.  These values in particular are often changed:

- useGrounding - formerly useW2V, turns on grounding, which uses Word2Vec (W2V)
- useCacheForOntologies - enables use of smaller and faster cached ontology files
- useCacheForW2V - enables use of smaller and faster vector files

Grounding no longer requires additional installation and in fact it is enabled by default.
The large vector file used for this purpose is declared as a library dependency and will be
installed automatically.  One disadvantage of these settings is that the program will start
up more slowly and require extra memory.  One way to avoid this is to turn off grounding by
setting `useGrounding` to `false`.  If you want to be relatively fast and still ground,
follow instructions below for [optimizing](#optimizing) to create cached versions of these files and then
turn on the two values `useCacheForOntologies` and `useCacheForW2V`.

Two previous settings are no longer used:

- useTimeNorm - activates time processing functions
- useGeoNorm - activates geolocation functions

Their functionalty has been incorporated into the setting for `entityFinders`
```
entityFinders = ["gazetteer", "rulebased", "geonorm", "timenorm"]
```
in which they are configured to be active.  To deactivate them, remove them from the list.
They no longer require any additional configuration.  All the code and resources are retrieved
from projects [timenorm](https://github.com/clulab/timenorm) and
[geonorm](https://github.com/clulab/geonorm), which are separate from
Eidos and declared in `built.sbt` as a library dependencies.

With `useGrounding` set to `true` and `geonorm` and `timenorm` included, your output should look more like this:

![Eidos with Grounding](/doc/grounding.png?raw=True")


## Optimizing

Processing the vector file and ontologies used in grounding can consume multiple minutes of time,
so if you want to run Eidos more than once with grounding, then it's useful to cache a preprocessed
version of all of them.  This requires a large amount of memory, possibly 8 or 10GB, so please read
the [notes](#notes) below.  If the configuration file `eidos.conf` is adjusted appropriately
(`useGrounding` is turned on) and there is enough memory, then the command
```bash
> sbt "runMain org.clulab.wm.eidos.apps.CacheOntologies"
```
should write serialized versions of the configured vector file and known ontologies to the directory
`./cache/`.  To then use the cached copies, set `useCacheForOntologies = true` in `eidos.conf`.
`useCacheForW2V` is set to track the same value, but can be independently controlled if need be.
The program should work significantly faster afterwards.


## Translating

Eidos is being translated into Portuguese!  The functionality is in the alpha stages.  To try it
out, switch `language` in `eidos.conf` from `english` to `portuguese`.  Here is a sneak preview:

![Eidos in Portuguese!](/doc/portuguese.png?raw=True")


## Integrating

If you want to use Eidos in its default configuration, it is available to project management
software like Maven and `sbt` in prepackaged form, independently of the GitHub repository.
Code like that below can be used to declare the dependency.
```scala
libraryDependencies ++=
  Seq(
    "org.clulab"    %% "eidos"          % "0.2.2"
  )
```

These instructions have otherwise assumed use of `sbt` which does significant management
of Java's $CLASSPATH settings, directing Java both to the compiled class files and the jar files
of library dependencies.  The easiest way to help Java use Eidos without direct aid of
`sbt` is to "assemble" a single jar file for Eidos:
```bash
> sbt assembly
```
This command should result in a large jar file being placed in a subdirectory of `target`
based on the version of Scala configured and the current Eidos version number.  The file
might be `target/scala-2.12/eidos-assembly-0.2.2-SNAPSHOT.jar`.  To access Eidos from Java,
add the assembled jar file to your $CLASSPATH, project, or command line.
```bash
> java -Xmx6g -classpath target/scala-2.12/eidos-assembly-0.2.2-SNAPSHOT.jar org.yourself.eidosClient.YourClassName
```

Another option is to "publish" the project locally, either in an Ivy repository as `sbt` prefers
with `publishLocal`, or a local Maven repository with `publishM2`.  This will result in a smaller jar
file for Eidos classes and a record of the library dependencies.
```bash
> sbt publishLocal
```
Eidos can then be used in other projects managed by `sbt` (or Maven) with the same instructions
used for non-local storage.
```scala
libraryDependencies ++=
  Seq(
    "org.clulab"    %% "eidos"          % "0.2.2-SNAPSHOT"
  )
```


# Notes

The default size of the memory allocation pool for the JVM is usually 1/4 of the
computer's physical memory, but Eidos may require more RAM than that.  It is currently
being developed and tested with a 6GB limit or more if timenorm or grounding is
configured.  Performance can suffer significantly if the Java garbage collector is
activated frequently.

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


# License

While we will soon be licensed as Apache, currently one dependency has a GPL
license.  This will be removed very soon and the license will be updated.
