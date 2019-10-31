package org.clulab.wm.eidos.apps.batch

import java.time.LocalDateTime

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.meta.EidosMetaUtils

object FilterWithMetaFromDirectory extends App {
  val inputDir = args(0)
  val metaDir = args(1)

  val converter = EidosMetaUtils.convertTextToMeta17k _

  def dctToString(dct: Option[DCT]): String = {
    dct.map { dct =>
      if (dct.interval.isDefined)
        dct.interval.start.toString + "-" + dct.interval.end.toString()
      else
        "<unknown>"
    }.getOrElse("<none>")
  }

  val cutoff = LocalDateTime.of(2017, 4, 1, 0, 0) // April 1, 2017
  val files = FileUtils.findFiles(inputDir, "txt")
  val reader = new EidosSystem()
  // TODO: KWA Some of this was to filter out documents, but it is long obsolete
//  val timenorm = reader.loadableAttributes.timenorm.get

  println("Good\t\t\tBad")
  println("txt\tmeta\ttime\tdct\ttxt\tmeta\ttime\tdct")

  files.foreach { file =>
    try {
      val meta = converter(metaDir, file)
      val json = EidosMetaUtils.getMetaData(converter, metaDir, file) // May not be there.
      val documentCreationTime = EidosMetaUtils.getDocumentCreationTime(json)
//      val dct = documentCreationTime.map { documentCreationTime =>
//        new DCT(timenorm.dct(timenorm.parse(documentCreationTime)), documentCreationTime)
//      }
//      val keep =
//        if (dct.isEmpty)
//          false
//        else {
//          val interval = dct.get.interval
//
//          interval.isDefined && dct.get.interval.start.isBefore(cutoff) && dct.get.interval.end.isBefore(cutoff)
//        }

      // So, exception defaults to do not keep.
//      if (!keep)
//        print("\t\t\t\t")
//      println(file.getName() + "\t" + meta.getName() + "\t" + documentCreationTime + "\t" + dctToString(dct))
    }
    catch {
      case exception: Exception =>
        println(s"Exception for file $file")
        exception.printStackTrace()
    }
  }
}
