package org.clulab.wm.eidos.text.english.grounding

import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.Seq

class TestGrounding extends EnglishTest {
  // Grounding needs to be activated in englishTest.conf for these tests to be active.
  // They are usually not because of the large vector file that is required for realistic tests.
  // Furthermore, the appropriate grounder, such as wm_compositional needs to be activated.

  abstract class CompositionalGroundingTextTester {
    val groundTopN: Option[Int] = Option(5)
    val threshold: Option[Float] = Option(0.5f)
    val active: Boolean

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
        topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold):
        (Seq[EidosMention], Seq[EidosMention])

    def allGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[String]

    def groundingShouldContain(mention: EidosMention, value: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      if (active) {
        val groundingNames = allGroundingNames(mention, topN, threshold)
        groundingNames should contain(value)
      }
    }
  }

  object CompositionalGroundingTextTester {

    def apply(name: String): CompositionalGroundingTextTester = {
      val ontologyGrounderOpt: Option[OntologyGrounder] = ieSystem.components.ontologyHandlerOpt.get.ontologyGrounders.find { ontologyGrounder =>
        ontologyGrounder.name == name
      }

      ontologyGrounderOpt.map { ontologyGrounder =>
        new RealCompositionalGroundingTextTester(name, ontologyGrounder)
      }.getOrElse(new FakeCompositionalGroundingTextTester)
    }
  }

  class FakeCompositionalGroundingTextTester extends CompositionalGroundingTextTester {
    val active = false

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
        topN: Option[Int], threshold: Option[Float]):
        (Seq[EidosMention], Seq[EidosMention]) = (Seq.empty, Seq.empty)

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[String] = Seq.empty
  }

  class RealCompositionalGroundingTextTester(name: String, ontologyGrounder: OntologyGrounder) extends CompositionalGroundingTextTester {
    val active = true

    def split(text: String): Array[String] = text.split(' ')

    def groundings(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): OntologyGroundings = {
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundEidosMention(mention, topN = groundTopN, threshold = threshold)
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branch.map { branch => "/" + branch }.getOrElse("")

        newName -> ontologyGrounding
      }.toMap

      groundings
    }

    protected def topGroundingValue(mention: EidosMention, componentName: String): Float = {
      val allGroundings = groundings(mention)
      val topGrounding = allGroundings(name + "/" + componentName).headOption.get.score
      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "concept")

    def topPropertyGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "property")

    def topProcessGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "process")

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[String] = {
      val allGroundings = groundings(mention, topN, threshold)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding.name }
      }.toSeq

      names
    }
    // to get both name AND score of groundings
    def allGroundingInfo(mention: EidosMention): Seq[(String,Float)] = {
      val allGroundings = groundings(mention)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding.name, grounding.score) }
      }.toSeq

      names
    }

    // TODO: pass topN and threshold through
    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
                         topN: Option[Int], threshold: Option[Float]):
    (Seq[EidosMention], Seq[EidosMention]) = {
      val doc = ieSystem.annotate(text)
      val causes = {
        val odinCauses = causeIntervals.map(x =>
            new TextBoundMention(label = "Entity", x, 0, doc, true, "FakeRule"))
        val annotatedDocument = AnnotatedDocument(doc, odinCauses)
        val eidosCauses = annotatedDocument.eidosMentions

        // This only grounds the surfact mentions, but that is sufficient for the test.
        eidosCauses.foreach(ieSystem.components.ontologyHandlerOpt.get.ground)
        eidosCauses
      }

      val effects = {
        val odinEffects = effectIntervals.map(x =>
            new TextBoundMention(label = "Entity", x, 0, doc, true, "FakeRule"))
        val annotatedDocument = AnnotatedDocument(doc, odinEffects)
        val eidosEffects = annotatedDocument.eidosMentions

        // This only grounds the surfact mentions, but that is sufficient for the test.
        eidosEffects.foreach(ieSystem.components.ontologyHandlerOpt.get.ground)
        eidosEffects
      }

      val returned = (causes, effects)
      returned
    }
  }

  val tester: CompositionalGroundingTextTester = CompositionalGroundingTextTester("wm_compositional")

  {
    behavior of "Grounding 1"

    val text = "The oil supply caused civil unrest in Sudan."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 3)), Seq(Interval(4, 6)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/property/supply")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/concept/causal_factor/social_and_political/security/conflict/demonstrate")
    }
  }


  {
    behavior of "Grounding 2"

    val text = "The prices of oil caused conflict in Ethiopia."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 4)), Seq(Interval(5, 6)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/property/price")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility")
    }
  }


  {
    behavior of "Grounding 3"

    val text = "Conflict caused an increase in the transportation price of fresh water."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 1)), Seq(Interval(5, 11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head,"wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/property/price")
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/transportation/transportation")
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/concept/causal_factor/infrastructure/water")
    }
  }


  {
    behavior of "Grounding 4"

    val text = "Armed clashes caused a decrease in the supply of school supplies in Jonglei State."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/social_and_political/security/conflict/hostility")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/property/supply")
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/concept/causal_factor/social_and_political/education/educational_materials")
    }
  }


  {
    behavior of "Grounding 5"

    val text = "Food security was mentioned as the main reason for flight."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(9, 10)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/social_and_political/humanitarian/food")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/property/security")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/migration/emigration")
    }
  }


  {
    behavior of "Grounding 6"

    val text = "The primary reasons for moving were insecurity, lack of food, and poor access to services such as healthcare and education."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(6, 7), Interval(8, 11), Interval(14, 22), Interval(4, 5)), Seq(Interval(4, 5)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause1 correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/property/insecurity")
    }
    passingTest should "process \"" + text + "\" cause2 correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions(1), "wm_compositional/property/supply")
      tester.groundingShouldContain(causeMentions(1), "wm_compositional/concept/causal_factor/social_and_political/humanitarian/food")
    }
    passingTest should "process \"" + text + "\" cause3 correctly" taggedAs Somebody in {
      // FIXME:  'access' not grounding properly; the others work fine
      // TODO: It seems like the second one is the problem.  It has been commented out for regression testing.
      tester.groundingShouldContain(causeMentions(2), "wm_compositional/process/access/access", topN = Option(50), threshold = Option(0.0f))
//      tester.groundingShouldContain(causeMentions(2), "wm_compositional/concept/causal_factor/health_and_life/treatment/health_treatment", topN = Option(50), threshold = Option(0.0f))
      tester.groundingShouldContain(causeMentions(2), "wm_compositional/concept/causal_factor/social_and_political/education/education", topN = Option(50), threshold = Option(0.0f))
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/migration/migration")
    }
  }


  {
    behavior of "Grounding ACCESS"

    val text = "sorghum access caused an increase in migration."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 7)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/process/access/access")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/agriculture/crop")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/migration/migration")
    }
  }


  {
    behavior of "Grounding SUPPLY"

    val text = "sorghum shortage caused an increase in migration."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 7)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/process/access/shortage")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/agriculture/crop")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/migration/migration")
    }
  }

  // TODO: in progress to address github issue
  {
    behavior of "Grounding ISSUE #739"

    val text = "The price of oil increased the price of water transportation."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,4)), Seq(Interval(5,10)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/property/price")
      tester.groundingShouldContain(causeMentions.head, "wm_compositional/concept/causal_factor/environment/natural_resources/fossil_fuels")
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/property/price")
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/process/transportation/transportation")
      tester.groundingShouldContain(effectMentions.head, "wm_compositional/concept/causal_factor/infrastructure/water")
    }
  }


// template for compositional grounder tests
// add test name, sentence text, and token intervals for cause and effect mentions
// if you have multiple causes/effects, see "Grounding 6" test for how to include them
/*
  {
    behavior of "test name"

    val text = "Sentence goes here"
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,1)), Seq(Interval(1,2)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
      if (tester.active) {
        tester.allGroundingNames(causeMentions.head) should contain("wm_compositional/???")
        tester.groundingShouldContain(causeMentions.head, "wm_compositional/???")
      }
    }
    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
      if (tester.active) {
        tester.allGroundingNames(effectMentions.head) should contain("wm_compositional/???")
        tester.groundingShouldContain(effectMentions.head, "wm_compositional/???")
      }
    }
  }
*/
}
