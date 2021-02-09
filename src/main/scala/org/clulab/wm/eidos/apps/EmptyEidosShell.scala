package org.clulab.wm.eidos.apps

import org.clulab.utils.CliReader
import org.clulab.utils.DefaultMenuItem
import org.clulab.utils.ExitMenuItem
import org.clulab.utils.HelpMenuItem
import org.clulab.utils.IdeReader
import org.clulab.utils.MainMenuItem
import org.clulab.utils.Menu

object EmptyEidosShell extends App {
  val lineReader = {
    val prompt = "(Eidos)>>> "
    // The CliReader does not work in IntelliJ on Windows, so this is a hack.
    // Include a command line parameter and the lineReader will be switched.
    if (args.length == 0) new CliReader(prompt, "user.home", ".eidosshellhistory")
    else new IdeReader(prompt)
  }
  val mainMenuItems = Seq(
    new HelpMenuItem(":help", "show commands"),
    new MainMenuItem(":reload", "reload grammar", (_: Menu, _: String) => { println("Reloading..."); true }),
    new ExitMenuItem(":exit", "exit system")
  )
  val defaultMenuItem = new DefaultMenuItem((_: Menu, text: String) => { println(s"""Processing "$text"..."""); true })
  val menu = new Menu("Welcome to the Eidos Shell!", lineReader, mainMenuItems, defaultMenuItem)

  menu.run()
}
