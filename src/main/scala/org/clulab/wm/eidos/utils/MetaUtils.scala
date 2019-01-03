package org.clulab.wm.eidos.utils

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.json4s.jackson.JsonMethods.parse
import org.json4s.{JField, JObject, JString, JValue}

object MetaUtils {

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
      val goodTitle = MetaUtils.getMetaValue(json, "title")

      goodTitle
    }
    documentTitle
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

  def getMetaData(converter: (String, File) => File, metaDir: String, textFile: File): Option[JValue] = {
    val file = converter(metaDir, textFile)
    val json = if (file.exists()) {
      val text = FileUtils.getTextFromFile(file)
      val json = parse(text)

      Some(json)
    }
    else None

    json
  }

  def convertTextToMeta17k(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + StringUtils.beforeFirst(StringUtils.afterLast(textFileName, '_'), '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToMeta(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToJsonld(jsonldDir: String, textFile: File): File = {
    val textFileName = textFile.getName()
    val jsonldFileName = jsonldDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".jsonld"

    new File(jsonldFileName)
  }
}
