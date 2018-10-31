package org.clulab.wm.eidos.apps

import java.io.File
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.DCT
import org.clulab.wm.eidos.utils.{FileUtils, Sourcer, StringUtils}
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{JField, JObject, JString, JValue}

object FilterWithMetaFromDirectory extends App {
  val inputDir = args(0)
  val metaDir = args(1)

  def getMetaValue(json: JValue, name: String): Option[String] = {
    val values: List[String] = for {
      JObject(child) <- json
      JField("MT", JObject(mt)) <- child
      JField("N", JString(n)) <- mt // name
      if n == name
      JField("V", JString(v)) <- mt // value
    } yield {
//      println(name + ": " + v)
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

  val converter = convertTextToMeta17k _

  def getMetaData(metaDir: String, textFile: File): Option[JValue] = {
    val file = converter(metaDir, textFile)
    val json = if (file.exists()) {
      val text = FileUtils.getTextFromFile(file)
      val json = parse(text)

      Some(json)
    }
    else None

    json
  }

  def getDocumentCreationTime(json: Option[JValue]): Option[String] = {
    val documentCreationTime = json.flatMap { json =>
      val okDate: Option[String] = getMetaValue(json, "spreadsheet date")
      val goodDate: Option[String] = okDate.orElse(getMetaValue(json, "creation date"))
      val betterDate: Option[String] = goodDate.orElse(getMetaValue(json, "publicationDate"))
      val bestDate: Option[String] =
        if (betterDate.isDefined) {
          val date = betterDate.get

          if (date.size >= 10 && date.take(2) == "D:") {
            val dateOnly: Option[String] = Some(date.drop(2).take(8))

            reformat(sanitize(dateOnly))
          }
          else if (date.contains('/'))
            Some(StringUtils.beforeFirst(date, ' ', true).replace('/', '-'))
          else betterDate
        }
        else betterDate

      bestDate
    }
    documentCreationTime.map(_ + ".")
  }

  def dctToString(dct: Option[DCT]): String = {
    dct.map { dct =>
      if (dct.interval.isDefined)
        dct.interval.start.toString + "-" + dct.interval.end.toString()
      else
        "<unknown>"
    }.getOrElse("<none>")
  }

  val cutoff = LocalDateTime.of(2017, 4, 1, 0, 0) // April 1, 2017
  val files = FileUtils.findFiles(inputDir, "txt")
  val reader = new EidosSystem()
  val timenorm = reader.timenorm.get

  println("Good\t\t\tBad")
  println("txt\tmeta\ttime\tdct\ttxt\tmeta\ttime\tdct")

  files.foreach { file =>
    try {
      val meta = converter(metaDir, file)
      val json = getMetaData(metaDir, file) // May not be there.
      val documentCreationTime = getDocumentCreationTime(json)
      val dct = documentCreationTime.map { documentCreationTime =>
        new DCT(timenorm.dct(timenorm.parse(documentCreationTime)), documentCreationTime)
      }
      val keep =
        if (dct.isEmpty)
          false
        else {
          val interval = dct.get.interval

          interval.isDefined && dct.get.interval.start.isBefore(cutoff) && dct.get.interval.end.isBefore(cutoff)
        }

      // So, exception defaults to do not keep.
      if (!keep)
        print("\t\t\t\t")
      println(file.getName() + "\t" + meta.getName() + "\t" + documentCreationTime + "\t" + dctToString(dct))
    }
    catch {
      case exception: Exception =>
        println(s"Exception for file $file")
        exception.printStackTrace()
    }
  }
}