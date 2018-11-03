package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}
import java.util.concurrent.ForkJoinPool

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{FileUtils, MetaUtils}

import scala.collection.parallel.ForkJoinTaskSupport

object FilteredExtractMetaFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val metaDir = args(2)
  var threads = args(3).toInt

  val converter = MetaUtils.convertTextToMeta17k _

  val intervals = Seq(
    (0,     0),
    (1,   999),
    (1000,  1999),
    (2000,  2999),
    (3000,  3999),
    (4000,  4999),
    (5000,  5999),
    (6000,  6999),
    (7000,  7999),
    (8000,  8999),
    (9000,  9999),
    (10000, 10999),
    (11000, 11999),
    (12000, 12999),
    (13000, 13999),
    (14000, 14999),
    (15000, 15999),
    (16000, 16999),
    (17000, 17999),
    (18000, 18999),
    (19000, 19999),
    (20000, 24999),
    (25000, 29999),

    (30000, 34999),
    (35000, 39999),

    (40000, 44999),
    (45000, 49999),

    (50000, 54999),
    (55000, 59999),

    (60000, 64999),
    (65000, 69999),

    (70000, 74999),
    (75000, 79999),

    (80000, 84999),
    (85000, 89999),

    (90000, 94999),
    (95000, 99999),
    (100000, 200000)
  )

  val reader = new EidosSystem()

  intervals.foreach { interval =>
    val min = interval._1
    val max = interval._2
    val filterOutputDir = s"$outputDir/$min-$max"

    new File(filterOutputDir).mkdirs()

    def filter (file: File): Boolean = min <= file.length() && file.length <= max

    val files = findFiles(inputDir, "txt").filter(filter)
    val parFiles = files.par

    val forkJoinPool = new ForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)

    parFiles.tasksupport = forkJoinTaskSupport

    parFiles.foreach { file =>
      var pw: PrintWriter = null

      try {
        // 1. Open corresponding output file
        println(s"Extracting from ${file.getName}")
        // 2. Get the input file contents
        val text = FileUtils.getTextFromFile(file)
        val json = MetaUtils.getMetaData(converter, metaDir, file)
        val documentCreationTime = MetaUtils.getDocumentCreationTime(json)
        val documentTitle = MetaUtils.getDocumentTitle(json)
        // 3. Extract causal mentions from the text
        val annotatedDocuments = Seq(reader.extractFromText(text, documentCreationTime = documentCreationTime))
        annotatedDocuments.head.document.id = documentTitle
        // 4. Convert to JSON
        val corpus = new JLDCorpus(annotatedDocuments, reader)
        val mentionsJSONLD = corpus.serialize()
        // 5. Write to output file
        val path = MetaUtils.convertTextToJsonld(filterOutputDir, file)
        pw = FileUtils.printWriterFromFile(path)
        pw.println(stringify(mentionsJSONLD, pretty = true))
      }
      catch {
        case exception: Exception =>
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
      finally {
        if (pw != null)
          pw.close()
      }
    }
  }
}