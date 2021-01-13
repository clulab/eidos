package org.clulab.wm.eidos.apps

import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.web.WebSerializer
import org.clulab.wm.eidos.utils.DisplayUtils.displayMentions
import org.clulab.wm.eidoscommon.utils.{CliReader, IdeReader}

import scala.collection.immutable.ListMap

/**
  * Interactive shell for demonstrating Eidos
  */

object EidosShell extends App {
  val reader = {
    val prompt = "(Eidos)>>> "

    if (args.length == 0) new CliReader(prompt, "user.home", ".eidosshellhistory")
    else new IdeReader(prompt)
  }
  val commands = ListMap(
    ":help" -> "show commands",
    ":reload" -> "reload grammar",
    ":exit" -> "exit system"
  )
  val eidosConfig: Config = EidosSystem.defaultConfig
  var ieSystem = new EidosSystem(eidosConfig)
  val webSerializer = new WebSerializer(ieSystem, eidosConfig)

  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }

  def processMenu: Boolean = {
    val continue = reader.readLineOpt().map { line =>
      if (line == ":exit") false
      else {
        line match {
          case ":help" => printCommands()
          case ":reload" => ieSystem = new EidosSystem(EidosSystem.defaultConfig, Some(ieSystem))
          case text => extractFrom(text)
        }
        true
      }
    }.getOrElse(false)

    continue
  }

  def extractFrom(text: String): Unit = {
    val annotatedDocument = ieSystem.extractFromText(text)
    val doc = annotatedDocument.document
    val mentions = annotatedDocument.odinMentions
    val sortedMentions = mentions.sortBy(m => (m.sentence, m.getClass.getSimpleName))

    webSerializer.serialize(annotatedDocument, cagRelevantOnly = true, "eidosshell.html")
    displayMentions(sortedMentions, doc, true)
  }

  println("\nWelcome to the Eidos Shell!")
  printCommands()
  while (processMenu) { }
}
