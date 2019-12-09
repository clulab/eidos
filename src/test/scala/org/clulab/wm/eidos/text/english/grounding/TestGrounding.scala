package org.clulab.wm.eidos.text.english.grounding

import java.util

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.struct.Interval
import org.clulab.wm.eidos.document.AnnotatedDocument
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
//  {
//    val text =
//      """
//        |"We are very worried to see that food insecurity is spreading beyond conflict areas as rising prices,
//        |impassable roads and dysfunctional markets are preventing many families, even those in towns and
//        |cities, from accessing food," said FAO Country Representative Serge Tissot.
//      """
//    val tester = new GroundingGraphTester(text)
//
//    val prices = NodeSpec("prices", Inc("rising"))
//    val roads = NodeSpec("impassable roads", Quant("impassable"))
////    val markets = NodeSpec("dysfunctional markets", Quant("dysfunctional"))
////    val families = NodeSpec("many families", Quant("many"), Dec("preventing"))
//
//    behavior of "Grounding NodeSpec"
//
//    passingTest should "process node 1 correctly" taggedAs Somebody in {
//      tester.test(prices) should be (successful)
//
//      if (active) {
//        (tester.topPropertyGrounding(prices) > 0.5f) should be (true)
//        tester.allGroundingNames(prices).contains("wm_compositional/property/price") should be (true)
//      }
//    }
//
//    passingTest should "process node 2 correctly" taggedAs Somebody in {
//      tester.test(roads) should be (successful)
//
//      if (active) {
//        (tester.topConceptGrounding(roads) > 0.0f) should be (true)
//        tester.allGroundingNames(roads).contains("wm_compositional/concept/causal_factor/infrastructure/road") should be (true)
//      }
//    }
//  }

  class GroundingTextTester {
    val name = "wm_compositional"

    // TODO: Account for groundTopN, threshold, etc.
    val ontologyGrounder: OntologyGrounder = ieSystem.components.ontologyHandler.ontologyGrounders.find { ontologyGrounder =>
      ontologyGrounder.name == name
    }.get

    def split(text: String): Array[String] = text.split(' ')

    def groundings(mention: EidosMention): OntologyGroundings = {
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundOntology(mention)
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branch.map { branch => "/" + branch }.getOrElse("")

        newName -> ontologyGrounding
      }.toMap

      groundings
    }

    protected def topGroundingValue(mention: EidosMention, componentName: String): Float = {
      val allGroundings = groundings(mention)
      val topGrounding = allGroundings(grounderName + "/" + componentName).headOption.get._2
      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "concept")

    def topPropertyGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "property")

    def topProcessGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "process")

    def allGroundingNames(mention: EidosMention): Seq[String] = {
      val allGroundings = groundings(mention)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding._1.name }
      }.toSeq

      names
    }
    // to get both name AND score of groundings
    def allGroundingInfo(mention: EidosMention): Seq[(String,Float)] = {
      val allGroundings = groundings(mention)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding._1.name, grounding._2) }
      }.toSeq

      names
    }
  }

  val tester = new GroundingTextTester

  {
    behavior of "Grounding 1"

    val text = "The oil supply caused civil unrest in Sudan."
    val doc = ieSystem.annotate(text)

    val causeTBM = new TextBoundMention(label="Entity", Interval(0,3), 0, doc, true, "FakeRule")
    val effectTBM = new TextBoundMention(label="Entity", Interval(4,6), 0, doc, true, "FakeRule")

    val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
    val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head

    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

    passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause).contains("wm_compositional/property/supply") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels") should be (true)
      }
    }
    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/demonstrate") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 2"

    val text = "The prices of oil caused conflict in Ethiopia."
    val doc = ieSystem.annotate(text)

    val causeTBM = new TextBoundMention(label="Entity", Interval(0,4), 0, doc, true, "FakeRule")
    val effectTBM = new TextBoundMention(label="Entity", Interval(5,6), 0, doc, true, "FakeRule")

    val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
    val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head

    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

    passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause).contains("wm_compositional/property/price") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels") should be (true)
      }
    }
    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 3"

    val text = "Conflict caused an increase in the transportation price of fresh water."
    val doc = ieSystem.annotate(text)

    val causeTBM = new TextBoundMention(label="Entity", Interval(0,1), 0, doc, true, "FakeRule")
    val effectTBM = new TextBoundMention(label="Entity", Interval(5,11), 0, doc, true, "FakeRule")

    val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
    val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head

    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

    passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
      }
    }
    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(effect).contains("wm_compositional/property/price") should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/process/transportation/transportation") should be (true)
        tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/infrastructure/water") should be (true)
      }
    }
  }


 {
   behavior of "Grounding 4"

   val text = "Armed clashes caused a decrease in the supply of school supplies in Jonglei State."
   val doc = ieSystem.annotate(text)

   val causeTBM = new TextBoundMention(label="Entity", Interval(0,2), 0, doc, true, "FakeRule")
   val effectTBM = new TextBoundMention(label="Entity", Interval(6,11), 0, doc, true, "FakeRule")

   val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
   val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head

   val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
   val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

   passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
     if (active) {
       tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility") should be (true)
     }
   }
   passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
     if (active) {
       tester.allGroundingNames(effect).contains("wm_compositional/property/supply") should be (true)
       tester.allGroundingNames(effect).contains("wm_compositional/concept/causal_factor/social_and_political/education/educational_materials") should be (true)
     }
   }
 }


  {
    behavior of "Grounding 5"

    val text = "Food security was mentioned as the main reason for flight."
    val doc = ieSystem.annotate(text)

    val causeTBM = new TextBoundMention(label="Entity", Interval(0,2), 0, doc, true, "FakeRule")
    val effectTBM = new TextBoundMention(label="Entity", Interval(9,10), 0, doc, true, "FakeRule")

    val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
    val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head

    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

    passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause).contains("wm_compositional/concept/causal_factor/social_and_political/humanitarian/food") should be (true)
        tester.allGroundingNames(cause).contains("wm_compositional/property/security") should be (true)
      }
    }
    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(effect).contains("wm_compositional/process/migration/emigration") should be (true)
      }
    }
  }


  {
    behavior of "Grounding 6"

    val text = "The primary reasons for moving were insecurity, lack of food, and poor access to services such as healthcare and education."
    val doc = ieSystem.annotate(text)

    val cause1TBM = new TextBoundMention(label="Entity", Interval(6,7), 0, doc, true, "FakeRule")
    val cause2TBM = new TextBoundMention(label="Entity", Interval(8,11), 0, doc, true, "FakeRule")
    val cause3TBM = new TextBoundMention(label="Entity", Interval(14,22), 0, doc, true, "FakeRule")
    val effectTBM = new TextBoundMention(label="Entity", Interval(4,5), 0, doc, true, "FakeRule")

    val cause1AD = AnnotatedDocument(doc, Seq(cause1TBM))
    val cause1 = ieSystem.components.ontologyHandler.process(cause1AD).eidosMentions.head

    val cause2AD = AnnotatedDocument(doc, Seq(cause2TBM))
    val cause2 = ieSystem.components.ontologyHandler.process(cause2AD).eidosMentions.head

    val cause3AD = AnnotatedDocument(doc, Seq(cause3TBM))
    val cause3 = ieSystem.components.ontologyHandler.process(cause3AD).eidosMentions.head

    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head

    passingTest should "process \"" + cause1TBM.document.text + "\" cause1 correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause1).contains("wm_compositional/property/insecurity") should be (true)
      }
    }
    passingTest should "process \"" + cause2TBM.document.text + "\" cause2 correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(cause2).contains("wm_compositional/property/supply") should be (true)
        tester.allGroundingNames(cause2).contains("wm_compositional/concept/causal_factor/social_and_political/humanitarian/food") should be (true)
      }
    }
    failingTest should "process \"" + cause3TBM.document.text + "\" cause3 correctly" taggedAs Somebody in {
      if (active) {
        // FIXME:  'access' not grounding properly; the others work fine
        tester.allGroundingNames(cause3).contains("wm_compositional/process/access/access") should be (true)
        tester.allGroundingNames(cause3).contains("wm_compositional/concept/causal_factor/health_and_life/treatment/health_treatment") should be (true)
        tester.allGroundingNames(cause3).contains("wm_compositional/concept/causal_factor/social_and_political/education/education") should be (true)
      }
    }
    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
      if (active) {
        tester.allGroundingNames(effect).contains("wm_compositional/process/migration/migration") should be (true)
      }
    }
  }

///// template for compositional grounder tests
//  {
//    behavior of "name_of_test"
//
//    val text = "Sentence"
//    val doc = ieSystem.annotate(text)
//
//    // intervals of cause and effect mention spans in sentence
//    val causeTBM = new TextBoundMention(label="Entity", Interval(0,2), 0, doc, true, "FakeRule")
//    val effectTBM = new TextBoundMention(label="Entity", Interval(9,10), 0, doc, true, "FakeRule")
//
//    val causeAD = AnnotatedDocument(doc, Seq(causeTBM))
//    val cause = ieSystem.components.ontologyHandler.process(causeAD).eidosMentions.head
//
//    val effectAD = AnnotatedDocument(doc, Seq(effectTBM))
//    val effect = ieSystem.components.ontologyHandler.process(effectAD).eidosMentions.head
//
//    // fill in path in ontology for all cause/effect mentions
//    passingTest should "process \"" + causeTBM.document.text + "\" cause correctly" taggedAs Somebody in {
//      if (active) {
//        tester.allGroundingNames(cause).contains("wm_compositional/") should be (true)
//      }
//    }
//    passingTest should "process \"" + effectTBM.document.text + "\" effect correctly" taggedAs Somebody in {
//      if (active) {
//        tester.allGroundingNames(effect).contains("wm_compositional/") should be (true)
//      }
//    }
//  }

}
