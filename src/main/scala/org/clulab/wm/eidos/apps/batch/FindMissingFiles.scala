package org.clulab.wm.eidos.apps.batch

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.json4s.JValue
import org.json4s.JsonAST.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.jackson.JsonMethods.parse

object FindMissingFiles extends App {
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

  def getMetaData(metaDir: String, textFile: File): Option[JValue] = {
    val textFileName = textFile.getName()
    val metaFileName = metaDir + "/" + beforeFirst(afterLast(textFileName, '_'), '.') + ".json"
    val file = new File(metaFileName)
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

    (50000, 54999) // ,
//    (55000, 59999),
//
//    (60000, 64999),
//    (65000, 69999),
//
//    (70000, 74999),
//    (75000, 79999),
//
//    (80000, 84999),
//    (85000, 89999),
//
//    (90000, 94999),
//    (95000, 99999)
  )

  val files = findFiles(inputDir, "txt")

  intervals.foreach { interval =>
    val min = interval._1
    val max = interval._2
    val filterOutputDir = s"$outputDir/$min-$max"

    def filter (file: File): Boolean = min <= file.length() && file.length <= max

    // For each file in the input directory:
    files.filter(filter).foreach { file =>

      try {
        val path = s"$filterOutputDir/${file.getName}.jsonld"
        val out = new File(path)
        if (!out.exists()) {
          println("" + min + "\t" + file.getName)
        }
      }
      catch {
        case exception: Exception =>
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
      finally {
      }
    }
  }
}
