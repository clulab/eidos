package org.clulab.wm.eidos.exporters

import java.io.PrintWriter

import org.clulab.odin.Attachment
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Decrease, Hedging, Increase, Negation, Quantification}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention

import scala.collection.mutable.ArrayBuffer

trait Exporter {
  def export(annotatedDocument: AnnotatedDocument): Unit
}

object Exporter {

  def apply(exporterString: String, filename: String, reader: EidosSystem, groundAs: Seq[String], topN: Int): Exporter = {
    exporterString match {
      case "jsonld" => JSONLDExporter(filename + ".jsonld", reader)
      case "serialized" => SerializedExporter(filename)
      case "grounding" => GroundingAnnotationExporter(filename + ".ground.csv", reader, groundAs, topN)
      case "reground" => new RegroundExporter(filename + ".jsonld", reader)
      case "debugGrounding" => new DebugGroundingExporter(filename)
      case _ => throw new NotImplementedError(s"Export mode $exporterString is not supported.")
    }
  }

  // This version is intended for use when all exports are to be output to the same file which has already
  // been opened and is accessible through the printWriter.
  def apply(exporterString: String, printWriter: PrintWriter, reader: EidosSystem, groundAs: Seq[String], topN: Int): Exporter = {
    exporterString match {
      case "incdec" => new IncDecExporter(printWriter)
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

  def numAtt(eidosMention: EidosMention, clazz: Class[_]): Int =
      eidosMention.odinMention.attachments.count(_.getClass == clazz)

  def numInc(eidosMention: EidosMention): Int = numAtt(eidosMention, classOf[Increase])

  def numDec(eidosMention: EidosMention): Int = numAtt(eidosMention, classOf[Decrease])

  def isInc(eidosMention: EidosMention): Boolean = numInc(eidosMention) >= numDec(eidosMention)

  def isDec(eidosMention: EidosMention): Boolean = !isInc(eidosMention)

  def isPromotion(cause: EidosMention, effect: EidosMention): Boolean = isInc(cause) == isInc(effect)

  def isInhibition(cause: EidosMention, effect: EidosMention): Boolean = !isPromotion(cause, effect)

  def poorMansIndra(cause: EidosMention, effect: EidosMention): String =
      if (isPromotion(cause, effect)) "PROMOTE" else "INHIBIT"

  def removeTabAndNewline(s: String): String = s.replaceAll("(\\n|\\t)", " ")
}
