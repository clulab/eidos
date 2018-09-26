package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.json4s.{JField, JObject, JString, JValue}
import org.json4s.jackson.JsonMethods.parse

object FilteredExtractMetaFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val metaDir = args(2)

  def afterLast(string: String, char: Char, all: Boolean = true): String = {
    val index = string.lastIndexOf(char)

    if (index < 0)
      if (all) string
      else ""
    else string.substring(index + 1)
  }

  def beforeFirst(string: String, char: Char, all: Boolean = true): String = {
    val index = string.indexOf(char)

    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index)
  }

  def getDate(json: JValue, name: String): Option[String] = {
    val values: List[String] = for {
      JObject(child) <- json
      JField("MT", JObject(mt)) <- child
      JField("N", JString(n)) <- mt // name
      if n == name
      JField("V", JString(v)) <- mt // value
    } yield {
      println(name + ": " + v)
      v
    }
    values.headOption
  }

  def sanitize(documentCreationTime: Option[String]): Option[String] = {
    if (documentCreationTime.isDefined)
      try {
        LocalDate.parse(documentCreationTime.get, DateTimeFormatter.BASIC_ISO_DATE).atStartOfDay()
        Some(documentCreationTime.get)
      }
      catch {
        case exception: Exception => exception.printStackTrace(); None
      }
    else None
  }

  def reformat(documentCreationTime: Option[String]): Option[String] =
      documentCreationTime.map(dct => dct.substring(0, 4) + "-" + dct.substring(4, 6) + "-" + dct.substring(6, 8))

  def getDocumentCreationTime(metaDir: String, textFile: File): Option[String] = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + beforeFirst(afterLast(textFileName, '_'), '.') + ".json"
    val file = new File(metaFileName)
    val documentCreationTime: Option[String] = if (file.exists()) {
      val text = FileUtils.getTextFromFile(file)
      val json: JValue = parse(text)
      val goodDate = getDate(json, "creation date")
      val betterDate =
          if (goodDate.isDefined) goodDate
          else getDate(json, "publicationDate")
      val bestDate =
        if (betterDate.isDefined) {
          val date = betterDate.get

          if (date.size >= 10 && date.take(2) == "D:") Some(date.drop(2).take(8))
          else if (date.size == 4 && date.forall(c => '0' <= c && c <= '9')) Some(date + "0101")
          else None
        }
        else
          betterDate
      bestDate
    }
    else
      None
    reformat(sanitize(documentCreationTime))
  }

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
    (95000, 99999)
  )

  val files = findFiles(inputDir, "txt")
  val reader = new EidosSystem()

  intervals.foreach { interval =>
    val min = interval._1
    val max = interval._2
    val filterOutputDir = s"$outputDir/$min-$max"

    new File(filterOutputDir).mkdirs()

    def filter (file: File): Boolean = min <= file.length() && file.length <= max

    // For each file in the input directory:
    files.filter(filter).foreach { file =>
      var pw: PrintWriter = null

      try {
        // 1. Open corresponding output file
        println(s"Extracting from ${file.getName}")
        // 2. Get the input file contents
        val text = FileUtils.getTextFromFile(file)
        val documentCreationTime = getDocumentCreationTime(metaDir, file)
        // 3. Extract causal mentions from the text
        val annotatedDocuments = Seq(reader.extractFromText(text, documentCreationTime = documentCreationTime))
        // 4. Convert to JSON
        val corpus = new JLDCorpus(annotatedDocuments, reader)
        val mentionsJSONLD = corpus.serialize()
        // 5. Write to output file
        val path = s"$filterOutputDir/${file.getName}.jsonld"
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
