package org.clulab.wm.eidos.utils


import org.clulab.odin.Attachment
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.export._
import org.clulab.wm.eidos.mentions.EidosMention

import scala.collection.mutable.ArrayBuffer

object ExportUtils {

  def getExporter(exporterString: String, filename: String, reader: EidosSystem, groundAs: Seq[String], topN: Int): Exporter = {
    exporterString match {
      case "jsonld" => JSONLDExporter(filename + ".jsonld", reader)
      case "mitre" => MitreExporter(filename + ".mitre.tsv", reader, filename, groundAs, topN)
      case "serialized" => SerializedExporter(filename)
      case "grounding" => GroundingExporter(FileUtils.printWriterFromFile(filename + ".ground.csv"), reader, groundAs, topN)
      case _ => throw new NotImplementedError(s"Export mode $exporterString is not supported.")
    }
  }

  def getModifier(mention: EidosMention): String = {
    def quantHedgeString(a: Attachment): Option[String] = a match {
      case q: Quantification => Some(f"Quant(${q.trigger.toLowerCase})")
      case h: Hedging => Some(f"Hedged(${h.trigger.toLowerCase})")
      case n: Negation => Some(f"Negated(${n.trigger.toLowerCase})")
      case _ => None
    }

    val attachments = mention.odinMention.attachments.map(quantHedgeString).toSeq.filter(_.isDefined)

    val modifierString = attachments.map(a => a.get).mkString(", ")
    modifierString
  }

  //fixme: not working -- always ;
  def getPolarity(mention: EidosMention): String = {
    val sb = new ArrayBuffer[String]
    val attachments = mention.odinMention.attachments
    val incTriggers = attachments.collect{ case inc: Increase => inc.trigger}
    val decTriggers = attachments.collect{ case dec: Decrease => dec.trigger}
    for (t <- incTriggers) sb.append(s"Increase($t)")
    for (t <- decTriggers) sb.append(s"Decrease($t)")

    sb.mkString(", ")
  }

  def poorMansIndra(cause: EidosMention, effect: EidosMention): String = {
    def numDec(em: EidosMention): Int = em.odinMention.attachments.collect{case dec: Decrease => dec}.size
    def numInc(em: EidosMention): Int = em.odinMention.attachments.collect{case inc: Increase => inc}.size

    val effectPolarity = if (numDec(effect) > numInc(effect)) -1 else 1
    val causePolarity = if (numDec(cause) > numInc(cause)) -1 else 1

    if (effectPolarity * causePolarity > 0) "PROMOTE" else "INHIBIT"
  }

  def removeTabAndNewline(s: String): String = s.replaceAll("(\\n|\\t)", " ")

}
