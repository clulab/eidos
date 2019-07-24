package org.clulab.wm.eidos.apps.batch

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.io.SyncFailedException
import java.nio.charset.StandardCharsets
import java.util.concurrent.ForkJoinPool

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.MetaUtils

import scala.collection.parallel.ForkJoinTaskSupport

object FilteredExtractMetaFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val metaDir = args(2)
  val threads = args(3).toInt

  val doneDir = inputDir + "/done"
  val converter = MetaUtils.convertTextToMeta _ // 17k _
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
    (100000, 199999),
    (200000, 299999)
  )
  val allFiles = findFiles(inputDir, "txt")
  val reader = new EidosSystem()

  intervals.foreach { interval =>
    val min = interval._1
    val max = interval._2
    val filterOutputDir = outputDir
//    val filterOutputDir = s"$outputDir/$min-$max"

    //new File(filterOutputDir).mkdirs()

    def filter (file: File): Boolean = min <= file.length() && file.length <= max

    val files = allFiles.filter(filter)
    val parFiles = files.par

    val forkJoinPool = new ForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)

    parFiles.tasksupport = forkJoinTaskSupport

    parFiles.foreach { file =>
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
        val corpus = new JLDCorpus(annotatedDocuments)
        val mentionsJSONLD = corpus.serialize()
        // 5. Write to output file
        val path = MetaUtils.convertTextToJsonld(filterOutputDir, file)

        // This is done pedantically so that the FileOutputStream is accessible.
        val fos = new FileOutputStream(path)
        val osw = new OutputStreamWriter(new BufferedOutputStream(fos), StandardCharsets.UTF_8.toString)

        new PrintWriter(osw).autoClose { pw =>
          pw.println(stringify(mentionsJSONLD, pretty = true))
          pw.flush()
          osw.flush()
          fos.flush()
          fos.getFD.sync()
        }
        // Now move the file to directory done
        val newPath = doneDir + "/" + file.getName
        file.renameTo(new File(newPath))
      }
      catch {
        case exception: SyncFailedException =>
          println(s"Synchronization failed for file $file")
          println("Exiting with code -2 on assumption that the disk is full")
          System.exit(-2)
        case exception: IOException =>
          println(s"IO failed for file $file")
          println("Exiting with code -2 on assumption that the disk is full")
          System.exit(-2)
        case exception: Exception =>
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
    }
  }
  println("I am exiting of my own free will!")
}
