package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.attachments.{CountAttachment, Location, Time}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.{FileUtils, TsvWriter}

case class MigrationExporter(filename: String) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    new TsvWriter(FileUtils.printWriterFromFile(filename)).autoClose { tsvWriter =>
      tsvWriter.println(
        "DocID",
        "Sentence Index",

        "TimeStart Text",
        "TimeStart Start",
        "TimeStart End",

        "TimeEnd Text",
        "TimeEnd Start",
        "TimeEnd End",

        "Time Text",
        "Time Start",
        "Time End",

        "Group Text",

        "Group Count Text",
        "Group Count Value",
        "Group Count Unit",

        "Group Modifier Text",
        "MoveTo Text",
        "MoveTo Location",
        "MoveFrom Text",
        "MoveFrom Location",
        "MoveThrough Text",
        "MoveThrough Location",
        "Sentence Text"
      )
      annotatedDocuments.foreach(printTableRows(_, tsvWriter))
    }
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, tsvWriter: TsvWriter): Unit = {

    def getEidosArgumentOpt(eidosMention: EidosMention, name: String): (Option[EidosMention], String) = {
      val argumentsOpt = eidosMention.eidosArguments.get(name)
      val argumentOpt = argumentsOpt.flatMap(_.headOption)
      val text = argumentOpt.map(_.odinMention.text).getOrElse("")

      (argumentOpt, text)
    }

    def getLocation(eidosMentionOpt: Option[EidosMention]): String = {
      eidosMentionOpt.flatMap { eidosMention: EidosMention =>
        eidosMention.odinMention.attachments.collectFirst { case locationAttachment: Location =>
          locationAttachment.geoPhraseID.geonameID.getOrElse("")
        }
      }.getOrElse("")
    }

    annotatedDocument.eidosMentions.foreach { eidosMention: EidosMention =>
      if (eidosMention.odinMention matches "HumanMigration") {
        val    (timeStartOpt,     timeStartText) = getEidosArgumentOpt(eidosMention, "timeStart")
        val      (timeEndOpt,       timeEndText) = getEidosArgumentOpt(eidosMention, "timeEnd")
        val         (timeOpt,          timeText) = getEidosArgumentOpt(eidosMention, "time")
        val        (groupOpt,         groupText) = getEidosArgumentOpt(eidosMention, "group")
        val               (_, groupModifierText) = getEidosArgumentOpt(eidosMention, "groupModifier")
        val        (moveToOpt,       moveToText) = getEidosArgumentOpt(eidosMention, "moveTo")
        val      (moveFromOpt,     moveFromText) = getEidosArgumentOpt(eidosMention, "moveFrom")
        val   (moveThroughOpt,  moveThroughText) = getEidosArgumentOpt(eidosMention, "moveThrough")

        val timeStartsAndEnds = Seq(timeStartOpt, timeEndOpt, timeOpt).map { eidosMentionOpt =>
          val (timeStart, timeEnd) = eidosMentionOpt.map { eidosMention =>
            eidosMention.odinMention.attachments.collectFirst { case time: Time =>
              val start = time.interval.intervals.headOption.map(_.startDate.toString).getOrElse("")
              val end = time.interval.intervals.lastOption.map(_.endDate.toString).getOrElse("")

              (start, end)
            }.getOrElse(("", ""))
          }.getOrElse(("", ""))

          (timeStart, timeEnd)
        }

        val (groupCountText, groupCountValue, groupCountUnit) = groupOpt.flatMap { group =>
          group.odinMention.attachments.collectFirst { case countAttachment: CountAttachment =>
            (countAttachment.text, countAttachment.migrationGroupCount.value.toString, countAttachment.migrationGroupCount.unit.toString)
          }
        }.getOrElse(("", "", ""))

        tsvWriter.println(
          annotatedDocument.document.id.getOrElse(""),
          eidosMention.odinMention.sentence.toString,

          timeStartText,
          timeStartsAndEnds(0)._1,
          timeStartsAndEnds(0)._2,

          timeEndText,
          timeStartsAndEnds(1)._1,
          timeStartsAndEnds(1)._2,

          timeText,
          timeStartsAndEnds(2)._1,
          timeStartsAndEnds(2)._2,

          groupText,

          groupCountText,
          groupCountValue,
          groupCountUnit,

          groupModifierText,
          moveToText,
          getLocation(moveToOpt),
          moveFromText,
          getLocation(moveFromOpt),
          moveThroughText,
          getLocation(moveThroughOpt),
          eidosMention.odinMention.sentenceObj.getSentenceText
        )
      }
    }
  }
}
