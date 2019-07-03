package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}

import org.apache.commons.text.StringEscapeUtils

import scala.collection.JavaConverters._


object SemEval2019Task12 {

  def main(args: Array[String]): Unit = args match {
    case Array("train", geoNamesIndexDir, modelFile, annDir) =>
      val annFiles = Files.newDirectoryStream(Paths.get(annDir), "*.ann").iterator.asScala
      val searcher = new GeoNamesSearcher(Paths.get(geoNamesIndexDir))
      val normalizer = GeoLocationNormalizer.train(searcher, annFiles.map(readTextAndGeoIdSpans))
      normalizer.save(Paths.get(modelFile))

    case Array("test", geoNamesIndexDir, modelFile, annDir) =>
      val k = 1
      val searcher = new GeoNamesSearcher(Paths.get(geoNamesIndexDir))
      val model = GeoLocationNormalizer.loadModel(Paths.get(modelFile))
      val normalizer = new GeoLocationNormalizer(searcher, Some(model))
      val results =
        for {
          annPath <- Files.newDirectoryStream(Paths.get(annDir), "*.ann").iterator.asScala
          (text, spans, geoIDs) = readTextAndGeoIdSpans(annPath)
          (span, geoID) <- spans zip geoIDs
        } yield {
          val predictedEntries = normalizer(text, span)
          val result = predictedEntries.map(_._1.id).take(k).contains(geoID)
          if (!result) {
            println(annPath)
            val (start, end) = span
            println(s"$geoID ${text.substring(start, end)}")
            for ((entry, score) <- predictedEntries) {
              println(s"${entry.id} ${entry.name} $score")
            }
            println()
          }
          result
        }
      val resultsList = results.toList
      val recallAtK = resultsList.count(x => x).toDouble / resultsList.size
      println(f"Recall@$k: ${100*recallAtK}%.2f%%")
  }

  def readTextAndGeoIdSpans(annFile: Path): (String, Seq[(Int, Int)], Seq[String]) = {
    val txtPath = Paths.get(annFile.toString.replace(".ann", ".txt"))
    val text = new String(Files.readAllBytes(txtPath))
    val annIdToSpan = scala.collection.mutable.Map.empty[String, (Int, Int)]
    val spanToGeoID = scala.collection.mutable.Map.empty[(Int, Int), String]
    val annText = new String(Files.readAllBytes(annFile)).trim().replaceAll("\n([^T#])", " $1")
    for (line <- annText.split("\r?\n")) line.split("\t", 3) match {
      case Array(annId, name, details) => annId.head match {
        case 'T' => name.split("""\s+""", 2) match {
          case Array(entityType, span) => entityType match {
            case "Location" =>
              val (start, end) = span.split("""[;\s]""") match {
                case Array(startString, endString) => (startString.toInt, endString.toInt)
                case Array(startString, _, _, endString) => (startString.toInt, endString.toInt)
              }
              val locationText = text.substring(start, end)
              if (locationText.noWhitespace != details.noWhitespace) {
                val phraseInTxt = StringEscapeUtils.escapeJava(locationText)
                val phraseInAnn = StringEscapeUtils.escapeJava(details)
                println(s"""WARNING: "$phraseInTxt" from .txt != "$phraseInAnn" from .ann""")
              }
              annIdToSpan(annId) = (start, end)
            case "Protein" => // do nothing
          }
        }
        case '#' => name.split("""\s+""") match {
          case Array("AnnotatorNotes", notedId) => annIdToSpan.get(notedId) match {
            case Some(span) => details match {
              case GeoIdMatch(geoId) => spanToGeoID(span) = geoId
            }
            case None => // do nothing
          }
        }
      }
    }
    val spans = annIdToSpan.values.toSeq.sorted
    (text, spans, spans.map(spanToGeoID))
  }

  private implicit class X(val string: String) {
    def noWhitespace: String = {
      string.replaceAll("""\s+""", "")
    }
  }

  private val GeoIdMatch = """.*<geoID>\s*(\S+)\s*</geoID>.*""".r
}
