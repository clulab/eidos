package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.MetaUtils

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable

object ExtractMetaKeysFromDirectory extends App {
  val metaDir = args(0)
  val files = findFiles(metaDir, "json")
  val keys = mutable.Set.empty[String]

  files.foreach { file =>
    try {
      println(s"Extracting from ${file.getName}")
      val jValueOpt = MetaUtils.getMetaData(file)

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