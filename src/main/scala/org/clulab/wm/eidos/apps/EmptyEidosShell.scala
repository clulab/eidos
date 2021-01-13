package org.clulab.wm.eidos.apps

import org.clulab.wm.eidoscommon.utils.CliReader
import org.clulab.wm.eidoscommon.utils.IdeReader
import org.clulab.wm.eidoscommon.utils.LineReader

class MenuItem(val command: MenuItem.Command) {

  def run(menu: Menu, input: String): Boolean = command(menu, input)
}

object MenuItem {
  type Command = (Menu, String) => Boolean
  val helpCommand: Command = (menu: Menu, _: String) => { menu.printCommands(); true }
  val exitCommand: Command = (_: Menu, _: String) => false
  val defaultCommand: Command = (_: Menu, input: String) => { println(s"Unknown command: $input"); true }
  val defaultMenuItem: DefaultMenuItem = new DefaultMenuItem(defaultCommand)
}

class KeyedMenuItem(val key: String, val hint: String, command: MenuItem.Command) extends MenuItem(command)

class HelpMenuItem(key: String, hint: String, command: MenuItem.Command = MenuItem.helpCommand) extends KeyedMenuItem(key, hint, command)

class ExitMenuItem(key: String, hint: String, command: MenuItem.Command = MenuItem.exitCommand) extends KeyedMenuItem(key, hint, command)

class DefaultMenuItem(command: MenuItem.Command = MenuItem.defaultCommand) extends MenuItem(command)

class Menu(greeting: String, lineReader: LineReader, keyedMenuItems: Seq[KeyedMenuItem], defaultMenuItem: DefaultMenuItem = MenuItem.defaultMenuItem) {

  def printCommands(): Unit = {
    println("COMMANDS:")
    keyedMenuItems.foreach { keyedMenuItem =>
      println(s"\t${keyedMenuItem.key}\t=> ${keyedMenuItem.hint}")
    }
  }

  def processMenu: Boolean = {
    println()
    lineReader.readLineOpt().map { line =>
      println()
      keyedMenuItems
          .find(_.key == line)
          .map(_.command(this, line))
          .getOrElse(defaultMenuItem.command(this, line))
    }.getOrElse(false)
  }

  def run(): Unit = {
    println(s"\n$greeting\n")
    printCommands()
    while (processMenu) { }
    println(s"Bye!\n")
  }
}

object EmptyEidosShell extends App {
  val lineReader = {
    val prompt = "(Eidos)>>> "
    // The CliReader does not work in IntelliJ on Windows, so this is a hack.
    // Include a command line parameter and the lineReader will be switched.
    if (args.length == 0) new CliReader(prompt, "user.home", ".eidosshellhistory")
    else new IdeReader(prompt)
  }
  val keyedMenuItems = Seq(
    new HelpMenuItem(":help", "show commands"),
    new KeyedMenuItem(":reload", "reload grammar", (_: Menu, _: String) => { println("Reloading..."); true }),
    new ExitMenuItem(":exit", "exit system")
  )
  val defaultMenuItem = new DefaultMenuItem((_: Menu, text: String) => { println(s"""Processing "$text"..."""); true })
  val menu = new Menu("Welcome to the Eidos Shell!", lineReader, keyedMenuItems, defaultMenuItem)

  menu.run()
}
