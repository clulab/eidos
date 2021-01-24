package org.clulab.wm.eidos.test

import com.typesafe.config.Config
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.scalatest.Tag

import scala.collection.Seq

object TestUtils {

  class TesterTag extends Tag("TesterTag")

  object Nobody   extends TesterTag
  object Somebody extends TesterTag
  object Keith    extends TesterTag
  object Becky    extends TesterTag
  object Egoitz   extends TesterTag
  object Ajay     extends TesterTag
  object Adarsh   extends TesterTag
  object Mithun   extends TesterTag
  object Fan      extends TesterTag
  object Zheng    extends TesterTag
  object Mihai    extends TesterTag
  object Ben      extends TesterTag
  object Heather  extends TesterTag
  object Vikas    extends TesterTag
  object George   extends TesterTag


  class CategoryTag  extends Tag("CategoryTag")

  object Contraption extends CategoryTag // For testing of infrastructure/scaffolding
  object Extraction  extends CategoryTag // For testing of rules


  class LanguageTag extends Tag("LanguageTag")

  object English    extends LanguageTag
  object Portuguese extends LanguageTag
  object Spanish    extends LanguageTag

  val successful: Seq[Nothing] = Seq()

  protected var mostRecentEidosSystemAndConfig: Option[(EidosSystem, Config)] = None

  // This is the standard way to extract mentions for testing
  def extractMentions(ieSystem: EidosSystem, text: String): Seq[Mention] = ieSystem.extractFromText(text, cagRelevantOnly = false).odinMentions

  def newEidosSystem(config: Config): EidosSystem = this.synchronized {
    val eidosSystem =
        if (mostRecentEidosSystemAndConfig.isEmpty || mostRecentEidosSystemAndConfig.get._2 != config)
          new EidosSystem(config)
        else
          mostRecentEidosSystemAndConfig.get._1

    mostRecentEidosSystemAndConfig = Some(eidosSystem, config)
    eidosSystem
  }
}
