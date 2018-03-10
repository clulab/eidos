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

  val ieSystem = new EidosSystem()

  println("\nWelcome to the Eidos Shell!")
  printCommands()

  var running = true

  while (running) {
    val line = reader.readLine
    line match {
      case ":help" =>
        printCommands()

      case ":reload" =>
        ieSystem.reload()

      case ":exit" | null =>
        running = false

      case text =>
        extractFrom(text)
    }
  }

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
    val populateSameAs = true
    val mentions = ieSystem.extractFrom(doc, populateSameAs = populateSameAs).sortBy(m => (m.sentence, m.getClass.getSimpleName))

    // debug display the mentions
    displayMentions(mentions, doc, true)
  }
}
