package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}

import org.apache.commons.text.StringEscapeUtils

import scala.collection.JavaConverters._

class SemEval2019Task12(geoNamesIndexDir: Path) {
  val searcher = new GeoNamesSearcher(geoNamesIndexDir)

  def features(text: String, spans: Seq[(Int, Int)]): Seq[Seq[(GeoNamesEntry, Array[Int])]] = {
    for ((start, end) <- spans) yield {
      val results = searcher(text.substring(start, end), 5)
      val scores = results.map(_._2).distinct.sorted.reverse
      val scoreRanks = scores.zipWithIndex.toMap
      for ((entry, score) <- results) yield {
        val scoreRank = scoreRanks(score)
        val logPopulation = math.log10(entry.population + 1).ceil.toInt
        (entry, Array(scoreRank, logPopulation))
      }
    }
  }

  def apply(text: String, spans: Seq[(Int, Int)]): Seq[Seq[GeoNamesEntry]] = {
    for ((start, end) <- spans) yield {
      searcher(text.substring(start, end), 5).map(_._1)
    }
  }
}

object SemEval2019Task12 {

  def main(args: Array[String]): Unit = args match {
    case Array("train", geoNamesIndexDir, annDir) =>
      val model = new SemEval2019Task12(Paths.get(geoNamesIndexDir))
      for {
        annPath <- Files.newDirectoryStream(Paths.get(annDir), "*.ann").iterator.asScala
        (text, spans, geoIDs) = readTextAndGeoIdSpans(annPath)
        (entryFeatures, geoID, (start, end)) <- (model.features(text, spans), geoIDs, spans).zipped
      } {
        println(text.substring(start, end))
        for ((entry, features) <- entryFeatures) {
          val label = if (entry.id == geoID) 1 else 0
          println(s"""$label ${features.mkString(" ")}""")
        }
        println()
      }


    case Array("test", geoNamesIndexDir, annDir) =>
      val k = 1
      val model = new SemEval2019Task12(Paths.get(geoNamesIndexDir))
      val results =
        for {
          annPath <- Files.newDirectoryStream(Paths.get(annDir), "*.ann").iterator.asScala
          (text, spans, geoIDs) = readTextAndGeoIdSpans(annPath)
          (predictedEntries, geoID, (start, end)) <- (model(text, spans), geoIDs, spans).zipped
        } yield {
          val result = predictedEntries.map(_.id).take(k).contains(geoID)
          if (!result && geoID != "NA") {
            println(annPath)
            println(s"$geoID ${text.substring(start, end)}")
            for (entry <- predictedEntries) {
              println(s"${entry.id} ${entry.name}")
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
                case Array(start, end) => (start.toInt, end.toInt)
                case Array(start, _, _, end) => (start.toInt, end.toInt)
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
