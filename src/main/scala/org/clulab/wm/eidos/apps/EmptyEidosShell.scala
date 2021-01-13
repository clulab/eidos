package org.clulab.wm.eidos.apps

import org.clulab.wm.eidoscommon.utils.CliReader
import org.clulab.wm.eidoscommon.utils.IdeReader

import scala.collection.immutable.ListMap

object EmptyEidosShell extends App {
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
          case ":reload" =>
          case text => println(s"""Processing "$text"...""")
        }
        true
      }
    }.getOrElse(false)

    continue
  }

  println("\nWelcome to the Eidos Shell!")
  printCommands()
  while (processMenu) { }
}
