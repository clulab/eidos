package org.clulab.wm.eidos.apps.shell

import com.typesafe.config.Config
import org.clulab.utils.CliReader
import org.clulab.utils.DefaultMenuItem
import org.clulab.utils.ExitMenuItem
import org.clulab.utils.HelpMenuItem
import org.clulab.utils.IdeReader
import org.clulab.utils.MainMenuItem
import org.clulab.utils.Menu
import org.clulab.wm.eidos.EidosApp
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.exporters.GroundingInsightExporter
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.web.WebSerializer
import org.clulab.wm.eidos.utils.DisplayUtils
import org.clulab.wm.eidos.utils.GroundingInfoSupplier

/**
  * Interactive shell for demonstrating Eidos
  */

object EidosShell extends EidosApp {

  class EidosGroundingInsight(eidosSystem: EidosSystem, config: Config) extends GroundingInfoSupplier {
    protected val groundingInsightExporter = new GroundingInsightExporter("", eidosSystem, config)

    def supplyGroundingInfo(m: EidosMention): String = groundingInsightExporter.mentionGroundingInfo(m)
  }

  val groundingInsights = false
  val eidosConfig: Config = EidosSystem.defaultConfig
  var ieSystem = new EidosSystem(eidosConfig)
  val webSerializer = new WebSerializer(ieSystem, eidosConfig)
  val eidosGroundingInsightOpt =
      if (groundingInsights) Some(new EidosGroundingInsight(ieSystem, config))
      else None

  def extractFromText(text: String): Unit = {
    val annotatedDocument = ieSystem.extractFromText(text)
    val doc = annotatedDocument.document
    val eidosMentions = annotatedDocument.eidosMentions
    val sortedMentions = eidosMentions.sortBy { eidosMention =>
      val m = eidosMention.odinMention
      (m.sentence, m.getClass.getSimpleName)
    }

    webSerializer.serialize(annotatedDocument, cagRelevantOnly = true, "eidosshell.html")
    DisplayUtils.displayEidosMentions(sortedMentions, doc, true, eidosGroundingInsightOpt)
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
