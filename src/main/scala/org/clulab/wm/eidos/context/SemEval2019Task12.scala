package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._

object SemEval2019Task12 {

  private val GeoIdMatch = """.*<geoID>\s*(\S+)\s*</geoID>.*""".r

  def readPhraseGeoIDs(annFile: Path): Seq[(String, String)] = {
    val phraseGeoIdPairs = scala.collection.mutable.Buffer.empty[(String, String)]
    val annIdToPhrase = scala.collection.mutable.Map.empty[String, String]
    val text = new String(Files.readAllBytes(annFile)).replaceAll("\n ", "  ")
    for (line <- text.split("\r?\n")) line.split("\t") match {
      case Array(annId, name, details) => annId.head match {
        case 'T' => name.split("""\s+""", 2) match {
          case Array(entityType, span) => entityType match {
            case "Location" => annIdToPhrase(annId) = details
            case "Protein" => // do nothing
          }
        }
        case '#' => name.split("""\s+""") match {
          case Array("AnnotatorNotes", annId) => annIdToPhrase.get(annId) match {
            case Some(phrase) => details match {
              case GeoIdMatch(geoId) => phraseGeoIdPairs += ((phrase, geoId))
            }
            case None => // do nothing
          }
        }
      }
    }
    phraseGeoIdPairs
  }

  def main(args: Array[String]): Unit = args match {
    case Array(geoNamesIndexDir, annDir) =>
      val k = 1
      val searcher = new GeoNamesSearcher(Paths.get(geoNamesIndexDir))
      val annDirs = Files.newDirectoryStream(Paths.get(annDir), "*.ann")
      val results = for ((phrase, geoID) <- annDirs.iterator.asScala.flatMap(readPhraseGeoIDs)) yield {

        // check if the correct ID is in the top k search results
        searcher(phrase, 5).take(k).map(_._1.id).contains(geoID)
      }
      val resultsList = results.toList
      val recallAtK = resultsList.count(x => x).toDouble / resultsList.size
      println(f"Recall@$k: ${100*recallAtK}%.2f%%")
  }
}
