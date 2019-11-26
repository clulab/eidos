package org.clulab.wm.eidos.text.english.grounding

import java.util.IdentityHashMap

import org.clulab.odin.Mention
import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
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
      val odinMentions = annotatedDocument.allOdinMentions
      val eidosMentions = annotatedDocument.allEidosMentions
      val odinToEidosMentionMap = new IdentityHashMap[Mention, EidosMention]

      eidosMentions.foreach { eidosMention =>
        odinToEidosMentionMap.put(eidosMention.odinMention, eidosMention)
      }

      odinMentions.foreach { odinMention =>
        if (!odinToEidosMentionMap.containsKey(odinMention))
          println("You messed up!")
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

    protected def topGroundingValue(nodeSpec: NodeSpec, componentName: String): Float = {
      val allGroundings = groundings(nodeSpec)
      val topGrounding = allGroundings(grounderName + "/" + componentName).headOption.get._2
      println("topGroundingValue:\t"+topGrounding)
      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(nodeSpec: NodeSpec): Float = topGroundingValue(nodeSpec, "concept")

    def topPropertyGrounding(nodeSpec: NodeSpec): Float = topGroundingValue(nodeSpec, "property")

    def topProcessGrounding(nodeSpec: NodeSpec): Float = topGroundingValue(nodeSpec, "process")

    def allGroundingNames(nodeSpec: NodeSpec): Seq[String] = {
      val allGroundings = groundings(nodeSpec)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding._1.name }
      }.toSeq

      names
    }
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
//    val markets = NodeSpec("dysfunctional markets", Quant("dysfunctional"))
//    val families = NodeSpec("many families", Quant("many"), Dec("preventing"))

    behavior of "Grounding"

    passingTest should "process node 1 correctly" taggedAs (Somebody) in {
      tester.test(prices) should be (successful)

      println("Top Property Grounding:\t"+tester.topPropertyGrounding(prices))
      println("All Grounding Names:\t"+tester.allGroundingNames(prices))

      if (active) {
        (tester.topPropertyGrounding(prices) > 0.5f) should be (true)
        tester.allGroundingNames(prices).contains("wm_compositional/property/price") should be (true)
      }
    }

    passingTest should "process node 2 correctly" taggedAs (Somebody) in {
      tester.test(roads) should be (successful)

      println("Top Concept Grounding:\t"+tester.topConceptGrounding(roads))
      println("All Grounding Names:\t"+tester.allGroundingNames(roads))

      if (active) {
        (tester.topConceptGrounding(roads) > 0.0f) should be (true)
        tester.allGroundingNames(roads).contains("wm_compositional/concept/causal_factor/infrastructure/road") should be (true)
      }
    }
  }
//
//  {
//    val text2 =
//      """
//        |The supply caused conflict in Sudan.
//        |The prices of oil caused conflict in Ethiopia.
//        |Conflict caused an increase in the transportation price of fresh water.
//        |Fighting caused an increase in the cost of transportation in Jonglei State.
//        |Armed clashes caused an increase in transportation demand in Jonglei State.
//      """
//    val tester = new GroundingGraphTester(text2)
//
//    val oilSupply = NodeSpec("supply")
//
//    behavior of "Grounding2"
//
//    passingTest should "process node 2 correctly" taggedAs (Somebody) in {
//      tester.test(oilSupply) should be (successful)
//
////      println("Top Concept Grounding:\t"+tester.topConceptGrounding(oilSupply))
////      println("All Grounding Names:\t"+tester.allGroundingNames(oilSupply))
//
//      if (active) {
////        (tester.topConceptGrounding(oilSupply) > 0.5f) should be (true)
//        tester.allGroundingNames(oilSupply).contains("wm/property/supply") should be (true)
//      }
//    }
//  }
}
