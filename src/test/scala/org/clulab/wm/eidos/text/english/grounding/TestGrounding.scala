package org.clulab.wm.eidos.text.english.grounding

import java.util.IdentityHashMap

import org.clulab.odin.Mention
import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.Seq

class TestGrounding extends EnglishTest {
  // Grounding needs to be activated in englishTest.conf for these tests to be active.
  // They are usually not because of the large vector file that is required for realistic tests.
  // Furthermore, the appropriate grounder, such as wm_compositional needs to be activated.
  val grounderName = "wm_compositional"
  val active: Boolean = {
    val grounders: Seq[OntologyGrounder] = this.ieSystem.components.ontologyHandler.ontologyGrounders

    grounders.exists { grounder => grounder.name == grounderName }
  }

  class GroundingGraphTester(text: String) extends GraphTester(text) {

    protected val odinToEidosMentionMap: IdentityHashMap[Mention, EidosMention] = {
      val eidosMentions = annotatedDocument.allEidosMentions
      val odinToEidosMentionMap = new IdentityHashMap[Mention, EidosMention]

     eidosMentions.foreach { eidosMention =>
        odinToEidosMentionMap.put(eidosMention.odinMention, eidosMention)
      }
      odinToEidosMentionMap
    }

    def groundings(nodeSpec: NodeSpec): OntologyGroundings = {
      val testResultOpt: Option[TestResult] = Option(testResults.get(nodeSpec))
      require(testResultOpt.isDefined) // The test should have already passed.
      require(testResultOpt.get.mention.isDefined) // Ditto
      val odinMention = testResultOpt.get.mention.get
      val eidosMentionOpt = Option(odinToEidosMentionMap.get(odinMention))
      require(eidosMentionOpt.isDefined)

      eidosMentionOpt.get.grounding
    }

    protected def topGrounding(nodeSpec: NodeSpec, componentName: String): Float = {
      val allGroundings = groundings(nodeSpec)
      val topGrounding = allGroundings(grounderName + "/" + componentName).headOption.get._2

      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(nodeSpec: NodeSpec): Float = topGrounding(nodeSpec, "concept")

    def topPropertyGrounding(nodeSpec: NodeSpec): Float = topGrounding(nodeSpec, "property")

    def topProcessGrounding(nodeSpec: NodeSpec): Float = topGrounding(nodeSpec, "process")
  }

  {
    val text =
      """
        |"We are very worried to see that food insecurity is spreading beyond conflict areas as rising prices,
        |impassable roads and dysfunctional markets are preventing many families, even those in towns and
        |cities, from accessing food," said FAO Country Representative Serge Tissot.
      """
    val tester = new GroundingGraphTester(text)

    val prices = NodeSpec("prices", Inc("rising"))
    val roads = NodeSpec("impassable roads", Quant("impassable"))
    val markets = NodeSpec("dysfunctional markets", Quant("dysfunctional"))
    val families = NodeSpec("many families", Quant("many"), Dec("preventing"))

    behavior of "Grounding"

    passingTest should "have ground node 1 correctly" taggedAs (Somebody) in {
      tester.test(prices) should be (successful)

      if (active)
        (tester.topPropertyGrounding(prices) > 0.5f) should be (true)
    }
  }
}
