# wmexchanger

This subproject of Eidos houses code that can be used to retrieve documents
from DART servers hosted by Two Six Labs.  It is similar to the elasticsearch
subproject that retrieves documents from that kind of server.  Retrieval
proceeds in two stages described in the next two sections.  The results need
to be transmitted back to the provider of the documents.  This is performed by
the RestProducerApp described in a third section.  Finally, this process has
been adapted to a hands-free environment in which these programs loop forever,
checking for new document and processing them as they appear.  Details about
the looping applications are provided in a fourth section.

Login credentials are required to access the servers.  They are not stored in
the source code.  Please ask for them if you need to run these programs.

## KafkaConsumerApp

Documents are "introduced" by a Kafka producer which is consulted by the Kafka
consumer included with the KafkaConsumerApp.  This App is run only on demand.
Demand is indicated when a new Kafka topic is "announced".  The announcement
might typically take place over email: "Hey readers, we have a new batch of
documents available under the topic 'drought' for you."

At this point, KafkaConsumerApp should be run.  Its first program argument is
the topic and would be specified as `app.topic=drought`.  The second argument
is the name of the directory where "introductions" should be stored.  It might
be `app.outputDir=../corpora/drought/kafka`.  Typically someone will have an
IntelliJ run configuration where this information can be recorded.  The main
class is `org.clulab.wm.wmconsumer.KafkaConsumerApp`.

These so-called "introductions" are JSON files which briefly summarize a larger
document.  Three fields are contained:

1. corpus-name: The name of the corpus, which likely matches the topic.
2. release-date: The date of the document in question.  The document may have changed over time.
3. cdr-data: A string containing JSON that if parsed would mimic a standard
CDR document except that for most keys, the corresponding value has been
replaced by a hash of the real value.  In this way a program might notice that
the value has already been read and is available locally.  Eidos does not use
this option.

The Kafka consumer in KafkaConsumerApp receives key-value pairs from the producer.
The key is a document ID and the value is the "introduction described above.
The App simply generates a filename from the id and stores the introductions into the
specified directory to be reused in the next stage.  The app does not know when the
collection of documents is complete and therefore when to stop.  After several
messages appear stating that 0 records were polled, press enter to stop the program.

For the nitty gritty, see the code and the resource `kafkaconsumer.properties`.

## RestConsumerApp

The "real" document, in CDR format, is provided by a REST API.  The call to
the REST service requires one piece of information, the document ID.  This is
recorded in the names of files produced in the previous stage.  Two other values
are optional: the release date and a boolean indicating whether annotations
should be provided.  The release date is recorded in the files from the
previous stage and Eidos does not use the annotations.

The RestConsumerApp which requests the documents over the API requires three
arguments:

1. inputDir: The directory containing the "introductions" such as `../corpora/drought/kafka`.
2. outputDir: The directory in which to store the CDRs, like `../corpora/drought/rest`.
3. doneDir: The directory where "introductions" should be moved once their corresponding
CDRs have been acquired.  `../corpora/draught/kafka/done` might be used.  The
directory should be created before the program runs.

These values along with the main class, `org.clulab.wm.wmconsumer.RestConsumerApp`
can be stored in a run configuration.  The program reads all the files from
inputDir, uses their filenames as document IDs, extracts the release-dates,
and asks via the API for the corresponding document.  These are placed in the
outputDir.  If successful, the input file is moved to the doneDir via renaming.
Both inputDir and doneDir should be located on the same physical device.  This
behavior allows one to monitor the program's progress and also restart it from
where it left off if problems arise.

For the nitty gritty, see the code and the resource `restconsumer.conf`.

After these two stages have completed, Eidos can be run on the resulting files,
the ones in outputDir, or `../corpora/drought/rest` in this example.

## Loop Apps

Each of the stages above has an endless loop variation that does not just run
until there is nothing more to do, but that does all that work and then waits
around forever in case more arrives.  There is a new fourth stage inserted between
the consumers and producer, and that is a version of the Eidos driver that does
the same, unlike any other existing driver.  The lineup is therefore

1. KafkaConsumerLoopApp
1. RestConsumerLoopApp
1. EidosLoopApp
1. RestProducerLoopApp

Each of these programs runs separately.  They communicate with each other by
sharing directories.  Because one program reading a file to be exchanged can't
know whether another program has finished writing it, a file locking mechanism
is used.  The writer of a file that gets read by another program writes the
file and then closes it.  Then it creates a matching lock file, indicating
that its OK for another program to read the file.  The other program will
eventually do that and then move the file to a "done" directory.  When the
writer program notices that there are lock files around without their "lockee",
it removes the lock file.  This should leave the shared directory empty, just
like it started.  Here is a tabular representation:

| Writer | Reader |
| --- | --- |
| 1. Remove dangling lock files | A. Check for lock files with a matching lockee |
| 2. Write a file for exchange, the lockee | B. Process the lockee |
| 3. Write the matching lock file | C. Move the lockee to the "done" directory |
| 4. Loop to 1 | D. Loop to A.

The shared directory might then have the states below.  Depending on how writer
and reader are synchronized, there could be numerous of the various kinds of
files present at any given time.

| Shared Directory | Action |
| --- | --- |
| empty | - |
| file.ext | 2 |
| file.ext<br>file.lock | 3, A, B|
| file.ext | C
| empty | 1 |

Although shared directories can be placed almost anywhere, these are the
conventional locations which can be used in Run/Debug Configurations in
IntelliJ or from the command line.  The base directory, named `$baseDir`
in the table, could be `../corpora/loop`.

| LoopApp | InputDir | OutputDir | DoneDir |
| --- | --- | --- | --- |
| 1. KafkaConsumerLoopApp | - | $baseDir/input/kafka | - |
| 2. RestConsumerLoopApp | $baseDir/input/kafka | $baseDir/input | $baseDir/input/kafka/done |
| 3. EidosLoopApp | $baseDir/input | $baseDir/output | $baseDir/input/done |
| 4. RestProducerLoopApp | $baseDir/output | - | $baseDir/output/done |

To facilitate testing, the consumer and producer functionalities have been
described in traits with implementations then extracted from the apps.  Mock
versions of the implementations are provided.  For the most part they simply
take files that had resulted from a previous run and move them around.  The
mock version of Eidos is slightly different in that it doesn't copy its output
from somewhere but instead performs the calculations for an empty document.
That functionality has required elsewhere and is exercized here.
