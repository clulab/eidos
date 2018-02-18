# eidos

Eidos is an open domain machine reading system designed by the Computational Language Understanding (CLU) Lab led by [Mihai Surdeanu](http://surdeanu.info/mihai/) at [University of Arizona](http://www.arizona.edu) for the World Modelers program.  Eidos uses a small set of [Odin](https://github.com/clulab/processors) rules to extract causal events from free text.

# Usage

## How to compile the source code

This is a standard sbt project, so use the usual commands, e.g., `sbt compile`, `sbt assembly`, to compile.
Add the generated jar files under `target/` to your $CLASSPATH, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

## How to use it

The eidos system is designed to be used in several ways:

### Using the scala API

(see [`src/main/scala/agro/demo/examples/ExtractFromText.scala`](https://github.com/clulab/eidos/blob/master/src/main/scala/agro/demo/examples/ExtractFromText.scala) for a complete running example)

```scala
import org.clulab.wm.EidosSystem
import utils.DisplayUtils.displayMention

val text = "Water trucking has decreased due to the cost of fuel."

// Initialize the reader
val reader = new EidosSystem()

// Extract the mentions
val mentions = reader.extractFrom(text)

// Display
mentions.foreach(displayMention)
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



### Extracting causal events from documents in a directory

``` 
sbt "runMain org.clulab.wm.ExtractFromDirectory /path/to/input/directory /path/to/output/directory"
```
Files in the input directory should end with `txt` and the extracted mentions from each file will be saved in corresponding JSON files.



### Running an interactive shell

The RAPShell (RAP = Representative Agricultural Pathway) is an interactive shell
for testing the output of Eidos. To run it, do

```
./shell
```

To run the webapp version of RAPShell locally, do:

```
sbt webapp/run
```

and then navigate to `localhost:9000` in a web browser.


## License

While we will soon be licensed as Apache, currently one dependency has a GPL licence.  This will be removed very soon and the license will be updated.



## Related resources:
Some materials related to this project can be found at https://drive.google.com/open?id=1cHJIfQTr0XE2CEqbo4POSm0-_xzrDP-A. Access is temporarily limited to UA account holders.  Other documents are included in the /doc directory of the repository.


## Notes:

The default size of the memory allocation pool for the JVM is 1/4 of your
physical memory, but eidos may require more RAM than that.  For some operating
systems (apparently not Windows), you can increase the
memory allocation by specifying it in the `.sbtopts` file in the `eidos`
directory (the one in which this README resides): 

```
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
