[![Build Status](https://travis-ci.org/clulab/eidos.svg?branch=master)](https://travis-ci.org/clulab/eidos)

# Eidos

Eidos is an open-domain machine reading system designed by the [Computational
Language Understanding (CLU) Lab](http://clulab.cs.arizona.edu) at [University
of Arizona](http://www.arizona.edu) for the World Modelers DARPA program.  It
uses a cascade of [Odin](https://github.com/clulab/processors) grammars to
extract events from free text.

Eidos identifies entities like "food insecurity" along with a growing list of arguments
on those entities, such as `increases` / `decreases` / `quantifications`.  It
subsequently detects events that occur between entities (directed causal events,
for example) as in "food insecurity causes increased migration".  The list of
arguments and events is updated frequently and is documented on the [JSON-LD](https://github.com/clulab/eidos/wiki/JSON-LD)
page.

**Contents**

- Running Eidos
  - Using the Sample Apps
    - [Webapp](#webapp)
    - [Interactive Shell](#interactive-shell)
    - [ExtractAndExport](#extractandexport)
    - [Other Apps](#other-apps)
  - Using the Scala API
    - [Prettified Display](#prettified-display)
    - [Export to JSON-LD](#export-to-json-ld)
- Connecting Eidos
  - [Indra](#indra)
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
included programs as described here.  `sbt` in turn requires Java, so that will need to have
been installed as well, but that's the extent of it.  Other software packages that Eidos depends
on will be downloaded automatically by `sbt`.  The contents of this repository can be downloaded
with `git` or via the "Clone or download" button on the project's GitHub page.  Assuming you
have done this and any necessary unzipping, you should have a directory called `eidos` containing
the file `build.sbt` among others.  `cd` to this directory.

## Using the Sample Apps

The software includes several sample applications that demonstrate its abilities.  Some are more
colorful and interactive and others are tailored for machine readable output.  They are fairly
large programs and the software is configured in `.jvmopts` to supply Java with lots of memory.
If you see complaints about inadequate heap space, please check the <a href="#notes">notes</a>
section for possible remedies.

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


You can now submit texts for Eidos to process.  Please note that the very first submission will
require extra time as lazily loaded parts of the system are initialized, but subsequent texts
will be processed much more quickly.

To eventually escape from `sbt`, you can stop the web server with Control-D and then quit the
program with `exit`.


### Interactive Shell

EidosShell is an interactive text-based program for testing the output of Eidos.  It's output is
less complete than the webapp, but it stands alone, no web browser is required, and consumes
fewer resources.  A script is provided to run it with

```bash
./shell
```
or
```DOS.bat
.\shell
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
> sbt "runMain org.clulab.wm.eidos.apps.ExtractFromDirectory /path/to/input/directory /path/to/output/directory"
```

or its two-line counterpart.  Again, files in the input directory should end with `.txt` and
the extracted mentions from each file will be saved in corresponding JSON-LD files.
**Note** that you cannot use tildes (`~`) in the invocation in lieu of the home directory for
this and most Java-like programs.


## Using the Scala API

One is of course not limited to using the existing apps: one can write new programs which
make use of Eidos's Scala interface.  New programs can be contained in the same `sbt` project
or a different one, in Java projects, in Python projects, or elsewhere.  See the
section on <a href="#integration">integration</a> for tips on how this is accomplished.
This description assumes that a program is added to the collection of existing apps in
their `apps` directory or somewhere nearby.  There are two major output formats provided
through the Scala API.

### Prettified Display

To produce a pretty display of the extracted mentions, use `EidosSystem` to extract
an `annotatedDocument` from the text and then display its mentions.

```scala
package your.package.name

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
sbt:eidos> runMain your.package.name.YourClassName
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
package your.package.name

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
  val corpus = new JLDCorpus(Seq(annotatedDocument), reader)
  val mentionsJSONLD = corpus.serialize()
  println(stringify(mentionsJSONLD, pretty = true))
}
```

The output that results when such code is called with

```bash
sbt:eidos> runMain your.package.name.YourClassName
```

can be [quite long](https://github.com/clulab/eidos/wiki/API-output-examples#json-ld-output).
The [JSON-LD export format](https://github.com/clulab/eidos/wiki/JSON-LD) for Eidos is defined
on a wiki page.


# Connecting Eidos
## Indra
## Delphi
# Working with Eidos
## Compiling

This is a standard [sbt](https://www.scala-sbt.org/) project, so use the usual
commands, e.g., `sbt compile` to compile or `sbt assembly` to create a jar file.
`sbt runMain` can be used to run some of the example applications directly, as
described below.  To access Eidos from Java, add the assembled jar file(s) under
`target/` to your $CLASSPATH.  A file like `eidos-assembly-0.1.6-SNAPSHOT.jar`
may suffice, depending on the build.  If necessary, see `build.sbt` for a list
of runtime dependencies. 

## Configuring
## Optimizing
## Translating
## Integrating
# Notes
# License

## How to compile the source code



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

The default size of the memory allocation pool for the JVM is 1/4 of your physical memory,
but Eidos may require more RAM than that.  It is currently being developed and tested with
a 6GB limit or 8GB if timenorm or grounding is configured.  Performance can suffer
significantly if the Java garbage collector is activated frequently, and lack of memory
is the usualy cause.

For those using `sbt`, the file `.jvmopts` is included with the source code to
arrange for more memory.  No other changes should be necessary.  Alternatively,
an environment variable specific to `sbt` can be used:

`SBT_OPTS=-Xmx6g`

If need be, a command line argument will work as well.
```bash
> sbt -J-Xmx8G
```

IDEs and other development tools are generally unaware of `.jvmopts`, but can be
configured via an environment variable instead.

`JAVA_TOOL_OPTIONS=-Xmx6g`

Other situations may require a more general setting.

`_JAVA_OPTIONS=-Xmx6g`

The procedure for defining these variables is dependent on operating system and
shell.
