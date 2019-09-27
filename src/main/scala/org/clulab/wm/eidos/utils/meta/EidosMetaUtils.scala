package org.clulab.wm.eidos.utils.meta

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.JsonAST.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.jackson.JsonMethods.parse

object EidosMetaUtils {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

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

  def getDocumentTitle(json: Option[JValue]): Option[String] = {
    val documentTitle = json.flatMap { json =>
      val goodTitle = EidosMetaUtils.getMetaValue(json, "title")

      goodTitle
    }
    documentTitle
  }

  def getDocumentCreationTimes(json: Option[JValue]): Seq[String] = {
    json.map { json =>
      val okDate: Option[String] = getMetaValue(json, "spreadsheet date")
      val goodDate: Option[String] = getMetaValue(json, "creation date")
      val betterDate: Option[String] = getMetaValue(json, "publicationDate")

      Seq(okDate, goodDate, betterDate).flatten
    }.getOrElse(Seq.empty[String])
  }

  def getDocumentCreationTime(json: Option[JValue]): Option[String] = {
    val documentCreationTime = json.flatMap { json =>
      val okDate: Option[String] = getMetaValue(json, "spreadsheet date")
      val goodDate: Option[String] = okDate.orElse(getMetaValue(json, "creation date"))
      val betterDate: Option[String] = goodDate.orElse(getMetaValue(json, "publicationDate"))
      val bestDate: Option[String] =
        if (betterDate.isDefined) {
          val date = betterDate.get

          if (date.length >= 10 && date.take(2) == "D:") {
            val dateOnly: Option[String] = Some(date.slice(2, 10))

            reformat(sanitize(dateOnly))
          }
          else if (date.contains('/'))
            Some(StringUtils.beforeFirst(date, ' ').replace('/', '-'))
          else betterDate
        }
        else betterDate

      bestDate
    }
    documentCreationTime.map(_ + ".")
  }

  def getMetaData(metaFile: File): Option[JValue] = {
    val json = if (metaFile.exists()) {
      val text = FileUtils.getTextFromFile(metaFile)
      val json = parse(text)

      Some(json)
    }
    else None

    json
  }

  def getMetaData(converter: (String, File) => File, metaDir: String, textFile: File): Option[JValue] = {
    val metaFile = converter(metaDir, textFile)

    getMetaData(metaFile)
  }

  def convertTextToMeta17k(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName
    val metaFileName = metaDir + "/" + StringUtils.beforeFirst(StringUtils.afterLast(textFileName, '_'), '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToMeta(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName
    val metaFileName = metaDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToJsonld(jsonldDir: String, textFile: File): File = {
    val textFileName = textFile.getName
    val jsonldFileName = jsonldDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".jsonld"

    new File(jsonldFileName)
  }
}
