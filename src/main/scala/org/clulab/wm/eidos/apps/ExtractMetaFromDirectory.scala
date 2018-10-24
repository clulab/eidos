package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{FileUtils, StringUtils, Timer}
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{JField, JObject, JString, JValue}

object ExtractMetaFromDirectory extends App {
  val inputDir = args(0)
  val metaDir = args(1)
  val outputDir = args(2)
  val timeFile = args(3)

  def getMetaValue(json: JValue, name: String): Option[String] = {
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


  def convertTextToMeta17k(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + StringUtils.beforeFirst(StringUtils.afterLast(textFileName, '_'), '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToMeta52(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".json"

    new File(metaFileName)
  }

  def getMetaData(metaDir: String, textFile: File): Option[JValue] = {
    val file = convertTextToMeta52(metaDir, textFile)
    val json = if (file.exists()) {
      val text = FileUtils.getTextFromFile(file)
      val json = parse(text)

      Some(json)
    }
    else None

    json
  }

  def getDocumentTitle(json: Option[JValue]): Option[String] = {
    val documentTitle = json.flatMap { json =>
      val goodTitle = getMetaValue(json, "title")

      goodTitle
    }
    documentTitle
  }

  def getDocumentCreationTime(json: Option[JValue]): Option[String] = {
    val documentCreationTime = json.flatMap { json =>
      val goodDate: Option[String] = getMetaValue(json, "creation date")
      val betterDate: Option[String] =
        if (goodDate.isDefined) goodDate
        else getMetaValue(json, "publicationDate")
      val bestDate: Option[String] =
        if (betterDate.isDefined) {
          val date = betterDate.get

          if (date.size >= 10 && date.take(2) == "D:") {
            val dateOnly: Option[String] = Some(date.drop(2).take(8))

            reformat(sanitize(dateOnly))
          } // + "T" + date.drop(10))
          else Some(date)
          //          else if (date.size == 4 && date.forall(c => '0' <= c && c <= '9')) Some(date + "0101")
          //          else None
        }
        else
          betterDate
      bestDate
    }
    documentCreationTime.map(_ + ".")
  }

  val files = findFiles(inputDir, "txt")

  Timer.time("Whole thing") {
    val timePrintWriter = FileUtils.printWriterFromFile(timeFile)
    timePrintWriter.println("File\tSize\tTime")
    val timer = new Timer("Startup")

    timer.start()
    // Prime it first.  This counts on overall time, but should not be attributed
    // to any particular document.
    val reader = new EidosSystem()
    reader.extractFromText("This is a test.")
    timer.stop()

    timePrintWriter.println("Startup\t0\t" + timer.elapsedTime.get)

    // When timing, do not do in parallel
    files.par.foreach { file =>
      var jsonldPrintWriter: PrintWriter = null

      try {
        // 1. Open corresponding output file
        println(s"Extracting from ${file.getName}")
        val timer = new Timer("Single file in parallel")
        val size = timer.time {
          // 2. Get the input file contents
          val text = FileUtils.getTextFromFile(file)
          val json = getMetaData(metaDir, file)
          val documentCreationTime = getDocumentCreationTime(json)
          val documentTitle = getDocumentTitle(json)
          // 3. Extract causal mentions from the text
          val annotatedDocuments = Seq(reader.extractFromText(text, documentCreationTime = documentCreationTime))
          annotatedDocuments.head.document.id = documentTitle
          // 4. Convert to JSON
          val corpus = new JLDCorpus(annotatedDocuments, reader)
          val mentionsJSONLD = corpus.serialize()
          // 5. Write to output file
          val path = s"$outputDir/${file.getName}.jsonld"
          jsonldPrintWriter = FileUtils.printWriterFromFile(path)
          jsonldPrintWriter.println(stringify(mentionsJSONLD, pretty = true))
          text.size
        }
        this.synchronized {
          timePrintWriter.println(file.getName() + "\t" + size + "\t" + timer.elapsedTime.get)
        }
      }
      catch {
        case exception: Exception =>
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
      finally {
        if (jsonldPrintWriter != null)
          jsonldPrintWriter.close()
      }
    }
    timePrintWriter.close()
  }
}