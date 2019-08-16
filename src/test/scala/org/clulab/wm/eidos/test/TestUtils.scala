package org.clulab.wm.eidos.test

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.graph
import org.clulab.wm.eidos.rule

import scala.collection.{Seq, mutable}

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


  val successful = Seq()

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

  class Test extends FlatSpec with Matchers {
    val passingTest = it
    val failingTest = ignore
    val brokenSyntaxTest = ignore
    val futureWorkTest = ignore // added to mark the tests that are not currently passing, but with planned changes to the
                                // framework, they will be achievable
    val inferenceTest = ignore  // type of futureWorkTest -- added for tests which are now failing because of entity
                                // filtering, basically because inference or coref would be needed
    val tempBrokenEntitiesTest = ignore
    val affectEventTest = ignore
    val waitingForProcessors = ignore  // type of futureWorkTest -- added for tests which are now failing because they where designed using a SNAPSHOT version of processors
  }

  class ContraptionTest extends Test

  class ExtractionTest(val ieSystem: EidosSystem, val config: Config) extends ContraptionTest {
    // These multiple configs are to ensure that the same config that was used to initialize the EidosSystem
    // is available to the test framework without EidosSystem needing to record it in some way.
    def this(config: Config = ConfigFactory.load("englishTest")) = this(newEidosSystem(config), config)

    class GraphTester(text: String) extends graph.GraphTester(ieSystem, text)

    class RuleTester(text: String) extends rule.RuleTester(ieSystem, text)

    def useTimeNorm = ieSystem.components.useTimeNorm
    def useGeoNorm = ieSystem.components.useGeoNorm

    def extractMentions(text: String): Seq[Mention] = TestUtils.extractMentions(ieSystem, text)
  }

  class EnglishTest(ieSystem: EidosSystem, config: Config) extends ExtractionTest(ieSystem, config) {
    def this(config: Config = ConfigFactory.load("englishTest")) = this(newEidosSystem(config), config)
  }

  class PortugueseTest(ieSystem: EidosSystem, config: Config) extends ExtractionTest(ieSystem, config) {
    def this(config: Config = ConfigFactory.load("portugueseTest")) = this(newEidosSystem(config), config)
  }
}
