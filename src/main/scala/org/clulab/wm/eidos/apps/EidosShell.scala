package org.clulab.wm.eidos.apps

import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.web.WebSerializer
import org.clulab.wm.eidos.utils.DisplayUtils.displayMentions
import org.clulab.wm.eidoscommon.utils.DefaultMenuItem
import org.clulab.wm.eidoscommon.utils.ExitMenuItem
import org.clulab.wm.eidoscommon.utils.HelpMenuItem
import org.clulab.wm.eidoscommon.utils.MainMenuItem
import org.clulab.wm.eidoscommon.utils.Menu
import org.clulab.wm.eidoscommon.utils.{CliReader, IdeReader}

/**
  * Interactive shell for demonstrating Eidos
  */

object EidosShell extends App {
  val eidosConfig: Config = EidosSystem.defaultConfig
  var ieSystem = new EidosSystem(eidosConfig)
  val webSerializer = new WebSerializer(ieSystem, eidosConfig)

  def extractFromText(text: String): Unit = {
    val annotatedDocument = ieSystem.extractFromText(text)
    val doc = annotatedDocument.document
    val mentions = annotatedDocument.odinMentions
    val sortedMentions = mentions.sortBy(m => (m.sentence, m.getClass.getSimpleName))

    webSerializer.serialize(annotatedDocument, cagRelevantOnly = true, "eidosshell.html")
    displayMentions(sortedMentions, doc, true)
  }

  def extractFromMenu(menu: Menu, text: String): Boolean = {
    extractFromText(text)
    true
  }

  def reloadFromMenu(menu: Menu, key: String): Boolean = {
    ieSystem = new EidosSystem(EidosSystem.defaultConfig, Some(ieSystem))
    true
  }

  val lineReader = {
    val prompt = "(Eidos)>>> "
    // The CliReader does not work in IntelliJ on Windows, so this is a hack.
    // Include a command line parameter and the lineReader will be switched.
    if (args.length == 0) new CliReader(prompt, "user.home", ".eidosshellhistory")
    else new IdeReader(prompt)
  }
  val mainMenuItems = Seq(
    new HelpMenuItem(":help", "show commands"),
    new MainMenuItem(":reload", "reload grammar", reloadFromMenu),
    new ExitMenuItem(":exit", "exit system")
  )
  val defaultMenuItem = new DefaultMenuItem(extractFromMenu)
  val menu = new Menu("Welcome to the Eidos Shell!", lineReader, mainMenuItems, defaultMenuItem)

  menu.run()
}
