package org.clulab.wm.eidos.apps

import java.time.LocalDateTime

import com.typesafe.config.ConfigValueFactory
import org.clulab.timenorm.scate.{SimpleInterval, SimpleIntervals, TimeNormScorer, UnknownInterval}
import org.clulab.timenorm.scate.TimeNormScorer.Timex
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.Metadata

import scala.io.Source
import collection.JavaConverters._


object EvalTimeNorm {

  protected def run(): Double = {
    val timeNormEvalDir = "/org/clulab/wm/eidos/english/context/timenormeval/"
    val goldStream = getClass.getResourceAsStream(s"$timeNormEvalDir/gold_timenorm.csv")
    val goldLines = Source.fromInputStream(goldStream).getLines
    goldLines.next() // Skip headers
    // Build a Map with the gold time expressions.
    // The key is tuple with the document name and the document creation time
    // The values is the a Seq with the time expressions (TimeNormScorer.Timex corresponding) in the document
    val goldTimex = (for ((goldLine, goldIdx) <- goldLines.toSeq.zipWithIndex) yield {
      goldLine.split(",").map(_.trim) match {
        case Array(docId, dct, startSpan, endSpan, timexStrg, startIntervalStr, endIntervalStr) =>
          val startInterval = LocalDateTime.parse(startIntervalStr)
          val endInterval = LocalDateTime.parse(endIntervalStr)
          val timeInterval = SimpleInterval(startInterval, endInterval)
          // timexStrg is added to the Timex id so it can be printed
          val timex = Timex(s"${docId}e$goldIdx '$timexStrg'", (startSpan.toInt, endSpan.toInt), timeInterval)
          (docId, dct, timex)
        case Array(docId, dct, startSpan, endSpan, timexStrg) =>
          val timeInterval = UnknownInterval()
          val timex = Timex(s"${docId}e$goldIdx '$timexStrg'", (startSpan.toInt, endSpan.toInt), timeInterval)
          (docId, dct, timex)
        case _ => throw new RuntimeException(s"Unexpected number of columns for row $goldIdx in gold_timenorm.csv")
      }
    }).groupBy(t => (t._1, t._2)).mapValues(_.map(_._3))

    // Configure eidos to apply only TimeNormFinder
    val config = EidosSystem.defaultConfig
    val newConfig = config
      .withValue("EidosSystem.finders", ConfigValueFactory.fromAnyRef(List("timenorm").asJava))
      .withValue("ontologies.useGrounding", ConfigValueFactory.fromAnyRef(false))
    val useNeuralParser: Boolean = newConfig.getBoolean("timenorm.useNeuralParser")
    val eidosSystem = new EidosSystem(newConfig)


    // For each docId in goldTimex keys get the, parse the document and extract the time expressions found
    val precisionRecalls = for ((docId, dct) <- goldTimex.keys.toSeq.sorted) yield {
      val docStream = getClass.getResourceAsStream(s"$timeNormEvalDir$docId.txt")
      val docText = Source.fromInputStream(docStream).getLines().mkString("\n")

      // Get a document of tokens
      val tokenizedDoc = eidosSystem.components.procOpt.get.mkDocument(docText, keepText = true)
      // if useNeuralParser, parse the document creation time
      // else set the DCT of the document
      if (useNeuralParser) {
        val metadata = Metadata(eidosSystem, Some(dct), None)
        metadata.attachToDoc(tokenizedDoc)
      } else tokenizedDoc.setDCT(dct)
      val doc = eidosSystem.annotateDoc(tokenizedDoc)

      // Build a Timex for each Time attachment
      val mentions = eidosSystem.extractMentionsFrom(doc)
      val predictTimex = mentions.flatMap(_.attachments).zipWithIndex.collect {
        case (time: Time, tidx: Int) =>
          val span = time.interval.span
          val timexStrg = docText.slice(span.start, span.end)
          val intervals = time.interval.intervals match {
            case Seq() => UnknownInterval()
            case Seq(interval) => SimpleInterval(interval.startDate, interval.endDate)
            case seqIntervals =>
              val simpleIntervals = seqIntervals.map { i => SimpleInterval(i.startDate, i.endDate) }
              SimpleIntervals(simpleIntervals)
          }
          // timexStrg is added to the Timex id so it can be printed
          Timex(s"${docId}e$tidx '$timexStrg'", (span.start, span.end), intervals)
      }
      // Score only normalizable time expressions, filter out undefined ones.
      val goldDefinedTimex = goldTimex((docId, dct)).filter(_.time.isDefined)
      val predictDefinedTimex = predictTimex.filter(_.time.isDefined)
      TimeNormScorer.intervalScores(goldDefinedTimex, predictDefinedTimex, verbose = true)
    }

    // Calculate the overall performance
    val (docPrecisions, docRecalls) = precisionRecalls.unzip
    val precisions = docPrecisions.flatten
    val recalls = docRecalls.flatten
    val precision = precisions.sum / precisions.length
    val recall = recalls.sum / recalls.length
    val fscore = 2 * precision * recall / (precision + recall)
    printf("Precision: %.3f\n", precision)
    printf("Recall: %.3f\n", recall)
    printf("F1: %.3f\n", fscore)
    fscore
  }

  def test(): Double = run()

  def main(args: Array[String]): Unit = run()
}
