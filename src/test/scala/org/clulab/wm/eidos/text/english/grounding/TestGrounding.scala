package org.clulab.wm.eidos.text.english.grounding

import java.util

import org.clulab.odin.Mention
import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
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

    protected val odinToEidosMentionMap: util.IdentityHashMap[Mention, EidosMention] = {
      val odinMentions = annotatedDocument.allOdinMentions
      val eidosMentions = annotatedDocument.allEidosMentions
      val odinToEidosMentionMap = new util.IdentityHashMap[Mention, EidosMention]

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
//      println("topGroundingValue:\t"+topGrounding)
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

  //TODO: do we still need these earlier tests?
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

    behavior of "Grounding NodeSpec"

    passingTest should "process node 1 correctly" taggedAs Somebody in {
      tester.test(prices) should be (successful)

      if (active) {
        (tester.topPropertyGrounding(prices) > 0.5f) should be (true)
        tester.allGroundingNames(prices).contains("wm_compositional/property/price") should be (true)
      }
    }

    passingTest should "process node 2 correctly" taggedAs Somebody in {
      tester.test(roads) should be (successful)

      if (active) {
        (tester.topConceptGrounding(roads) > 0.0f) should be (true)
        tester.allGroundingNames(roads).contains("wm_compositional/concept/causal_factor/infrastructure/road") should be (true)
      }
    }
  }

  class GroundingTextTester {
    val name = "wm_compositional"

    // TODO: Account for groundTopN, threshold, etc.
    val ontologyGrounder: OntologyGrounder = ieSystem.components.ontologyHandler.ontologyGrounders.find { ontologyGrounder =>
      ontologyGrounder.name == name
    }.get

    def split(text: String): Array[String] = text.split(' ')

    def groundings(strings: Array[String]): OntologyGroundings = {
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundStrings(strings)
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branch.map { branch => "/" + branch }.getOrElse("")

        newName -> ontologyGrounding
      }.toMap

      groundings
    }

    protected def topGroundingValue(strings: Array[String], componentName: String): Float = {
      val allGroundings = groundings(strings)
      val topGrounding = allGroundings(grounderName + "/" + componentName).headOption.get._2
//      println("topGroundingValue:\t"+topGrounding)
      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(strings: Array[String]): Float = topGroundingValue(strings: Array[String], "concept")

    def topPropertyGrounding(strings: Array[String]): Float = topGroundingValue(strings: Array[String], "property")

    def topProcessGrounding(strings: Array[String]): Float = topGroundingValue(strings: Array[String], "process")

    def allGroundingNames(strings: Array[String]): Seq[String] = {
      val allGroundings = groundings(strings)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding._1.name }
      }.toSeq

      names
    }
    // to get both name AND score of groundings
    def allGroundingInfo(strings: Array[String]): Seq[(String,Float)] = {
      val allGroundings = groundings(strings)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding._1.name, grounding._2) }
      }.toSeq

      names
    }
  }

  val tester = new GroundingTextTester

  {
    behavior of "Grounding 1"

    val text = "roads"

    passingTest should "process \"" + text.mkString(" ") + "\" correctly" taggedAs Somebody in {
      val roads = tester.split(text)

      if (active) {
        (tester.topConceptGrounding(roads) > 0.6f) should be (true)
        tester.allGroundingNames(roads).contains("wm_compositional/concept/causal_factor/infrastructure/road") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 2"

    val text = "The oil supply caused civil unrest in Sudan."
    val cause = tester.split("The oil supply")
    val effect = tester.split("civil unrest")

    passingTest should "process \"" + cause.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topPropertyGrounding(cause) > 0.6f) should be (true)
        (tester.topConceptGrounding(cause) > 0.6f) should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/property/supply") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels") should be (true)
      }
    }
    passingTest should "process \"" + effect.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topConceptGrounding(effect) > 0.6f) should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/demonstrate") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 3"

    val text = "The prices of oil caused conflict in Ethiopia."
    val cause = tester.split("The prices of oil")
    val effect = tester.split("conflict")

    passingTest should "process \"" + cause.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topPropertyGrounding(cause) > 0.6f) should be (true)
        (tester.topConceptGrounding(cause) > 0.6f) should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/property/price") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels") should be (true)
      }
    }
    passingTest should "process \"" + effect.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topConceptGrounding(effect) > 0.6f) should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 4"

    val text = "Conflict caused an increase in the transportation price of fresh water."
    val cause = tester.split("Conflict")
    val effect = tester.split("the transportation price of fresh water")

//    println("\n"+effect.mkString(" "))
//    println("Effect Groundings:\t"+tester.allGroundingInfo(effect).mkString("\n"))

    passingTest should "process \"" + cause.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topConceptGrounding(cause) > 0.6f) should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
      }
    }
    passingTest should "process \"" + effect.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topPropertyGrounding(effect) > 0.6f) should be (true)
        //FIXME: transportation fails bc its score is 0.579 (which is < 0.6 required here)
        // BUT it is still the top grounding for process
        (tester.topProcessGrounding(effect) > 0.6f) should be (true)
        (tester.topConceptGrounding(effect) > 0.6f) should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/property/price") should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/process/transportation/transportation") should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/infrastructure/water") should be (true)
      }
    }
  }


 {
   behavior of "Grounding 5"

   val text = "Armed clashes caused a decrease in the supply of school supplies in Jonglei State."
   val cause = tester.split("Armed clashes")
   val effect = tester.split("the supply of school supplies")

   passingTest should "process \"" + cause.mkString(" ") + "\" correctly" taggedAs Somebody in {
     if (active) {
       (tester.topConceptGrounding(cause) > 0.6f) should be (true)
       tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
     }
   }
   passingTest should "process \"" + effect.mkString(" ") + "\" correctly" taggedAs Somebody in {
     if (active) {
       (tester.topPropertyGrounding(effect) > 0.6f) should be (true)
       (tester.topConceptGrounding(effect) > 0.6f) should be (true)
       tester.allGroundingNames(effect).contains("wm_compositional/property/supply") should be (true)
       tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/education/educational_materials") should be (true)
     }
   }
 }


  {
    behavior of "Grounding 6"

    val text = "Food security was mentioned as the main reason for flight."
    val cause = tester.split("Food security")
    val effect = tester.split("flight")

    println("\n"+effect.mkString(" "))
    println("Effect Groundings:\t"+tester.allGroundingInfo(effect).mkString("\n"))

    passingTest should "process \"" + cause.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        (tester.topPropertyGrounding(cause) > 0.6f) should be (true)
        (tester.topConceptGrounding(cause) > 0.6f) should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/humanitarian/food") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/property/security") should be (true)
      }
    }
    passingTest should "process \"" + effect.mkString(" ") + "\" correctly" taggedAs Somebody in {
      if (active) {
        //FIXME:  flight fails bc its score is 0.578 (which is < 0.6 required here)
        // BUT it is still the top grounding for process
        (tester.topProcessGrounding(effect) > 0.6f) should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/process/migration/emigration") should be (true)
      }
    }
  }
}
