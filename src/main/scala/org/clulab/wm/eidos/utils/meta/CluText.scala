package org.clulab.wm.eidos.utils.meta

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.JsonAST.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.jackson.JsonMethods

class CluText(eidosSystem: EidosSystem, text: String, jValueOpt: Option[JValue]) extends EidosText {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  protected val metadata: Metadata = {
    val dctStringOpt: Option[String] = getDocumentCreationTime
    val titleOpt: Option[String] = getDocumentTitle

    Metadata(eidosSystem, dctStringOpt, titleOpt)
  }

  override def getText: String = text

  override def getMetadata: Metadata = metadata

  protected def sanitize(documentCreationTime: Option[String]): Option[String] = {
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

  protected def reformat(documentCreationTime: Option[String]): Option[String] =
    documentCreationTime.map(dct => dct.substring(0, 4) + "-" + dct.substring(4, 6) + "-" + dct.substring(6, 8))

  protected def getDocumentTitle: Option[String] = {
    val documentTitle = jValueOpt.flatMap { json =>
      val goodTitle = CluText.getMetaValue(json, "title")

      goodTitle
    }
    documentTitle
  }

  protected def getDocumentCreationTime: Option[String] = {
    val documentCreationTime = jValueOpt.flatMap { json =>
      val okDate: Option[String] = CluText.getMetaValue(json, "spreadsheet date")
      val goodDate: Option[String] = okDate.orElse(CluText.getMetaValue(json, "creation date"))
      val betterDate: Option[String] = goodDate.orElse(CluText.getMetaValue(json, "publicationDate"))
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
}

object CluText {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  def apply(eidosSystem: EidosSystem, textFile: File, metaFileOpt: Option[File]): CluText = {
    val text = FileUtils.getTextFromFile(textFile)
    val jValueOpt = metaFileOpt.map { file =>
      getJValue(file)
    }

    new CluText(eidosSystem, text, jValueOpt)
  }

  def getJValue(file: File): JValue = {
    val json = FileUtils.getTextFromFile(file)

    JsonMethods.parse(json)
  }

  def getDocumentCreationTimes(file: File): Seq[String] = {
    val json = FileUtils.getTextFromFile(file)
    val jValue = JsonMethods.parse(json)

    getDocumentCreationTimes(Option(jValue))
  }

  def convertTextToMeta17k(textFile: File, metaDir: String): File = {
    val newName = StringUtils.afterLast(textFile.getName, '_')

    FileEditor(textFile).setDir(metaDir).setName(newName).setExt("json").get
  }

  def convertTextToMeta(textFile: File, metaDir: String): File = {
    FileEditor(textFile).setDir(metaDir).setExt("json").get
  }

  def convertTextToJsonld(textFile: File, jsonldDir: String): File = {
    FileEditor(textFile).setDir(jsonldDir).setExt("jsonld").get
  }

  protected def getMetaValue(json: JValue, name: String): Option[String] = {
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

  protected def getDocumentCreationTimes(jValueOpt: Option[JValue]): Seq[String] = {
    jValueOpt.map { json =>
      val okDate: Option[String] = getMetaValue(json, "spreadsheet date")
      val goodDate: Option[String] = getMetaValue(json, "creation date")
      val betterDate: Option[String] = getMetaValue(json, "publicationDate")

      Seq(okDate, goodDate, betterDate).flatten
    }.getOrElse(Seq.empty[String])
  }
}
