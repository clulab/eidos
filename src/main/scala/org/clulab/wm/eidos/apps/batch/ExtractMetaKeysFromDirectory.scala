package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.meta.EidosMetaUtils
import org.json4s.JValue
import org.json4s.JsonAST.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString

import scala.collection.mutable

object ExtractMetaKeysFromDirectory extends App {
  val metaDir = args(0)
  val files = findFiles(metaDir, "json")
  val keys = mutable.Set.empty[String]

  files.foreach { file =>
    try {
      println(s"Extracting from ${file.getName}")
      val jValueOpt = EidosMetaUtils.getMetaData(file)

      jValueOpt.foreach { jValue: JValue =>
        val newKeys = for {
          JObject(mt) <- jValue
          JField("N", JString(key)) <- mt
        } yield
          key.toString

        keys ++= newKeys
        println(keys)
      }
    }
    catch {
      case exception: Exception =>
        println(s"Exception for file $file")
        exception.printStackTrace()
    }
  }
  println(keys)
}
