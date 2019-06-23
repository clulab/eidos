package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._

class SemEval2019Task12(geoNamesIndexDir: Path) {
  val searcher = new GeoNamesSearcher(geoNamesIndexDir)

  def apply(text: String, spans: Seq[(Int, Int)]): Seq[Seq[String]] = {
    for ((start, end) <- spans) yield {
      searcher(text.substring(start, end), 5).map(_._1.id)
    }
  }
}

object SemEval2019Task12 {

  def main(args: Array[String]): Unit = args match {
    case Array(geoNamesIndexDir, annDir) =>
      val k = 1
      val model = new SemEval2019Task12(Paths.get(geoNamesIndexDir))
      val results =
        for {
          annPath <- Files.newDirectoryStream(Paths.get(annDir), "*.ann").iterator.asScala
          (spans, geoIDs) = readPhraseGeoIDs(annPath)
          txtPath = Paths.get(annPath.toString.replace(".ann", ".txt"))
          text = new String(Files.readAllBytes(txtPath))
          (predictedIDs, geoID) <- model(text, spans) zip geoIDs
        } yield {
          predictedIDs.take(k).contains(geoID)
        }
      val resultsList = results.toList
      val recallAtK = resultsList.count(x => x).toDouble / resultsList.size
      println(f"Recall@$k: ${100*recallAtK}%.2f%%")
  }

  def readPhraseGeoIDs(annFile: Path): (Seq[(Int, Int)], Seq[String]) = {
    val annIdToSpan = scala.collection.mutable.Map.empty[String, (Int, Int)]
    val spanToGeoID = scala.collection.mutable.Map.empty[(Int, Int), String]
    val text = new String(Files.readAllBytes(annFile)).replaceAll("\n ", "  ")
    for (line <- text.split("\r?\n")) line.split("\t") match {
      case Array(annId, name, details) => annId.head match {
        case 'T' => name.split("""\s+""", 2) match {
          case Array(entityType, span) => entityType match {
            case "Location" => span.split("""[;\s]""") match {
              case Array(start, end) => annIdToSpan(annId) = (start.toInt, end.toInt)
              case Array(start, _, _, end) => annIdToSpan(annId) = (start.toInt, end.toInt)
            }
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
    (spans, spans.map(spanToGeoID))
  }

  private val GeoIdMatch = """.*<geoID>\s*(\S+)\s*</geoID>.*""".r
}
