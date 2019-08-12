package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.displayMentions
import org.clulab.wm.eidos.utils.{CliReader, IdeReader}

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

  var ieSystem = new EidosSystem()

  println("\nWelcome to the Eidos Shell!")
  printCommands()

  def processMenu: Boolean = {
    val continue = Option(reader.readLine).map { line =>
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

  while (processMenu) { }

  // summarize available commands
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }

  def extractFrom(text:String): Unit = {

    // preprocessing
    val doc = ieSystem.annotate(text)

    // extract mentions from annotated document
    val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))

    // debug display the mentions
    displayMentions(mentions, doc, true)
  }
}
