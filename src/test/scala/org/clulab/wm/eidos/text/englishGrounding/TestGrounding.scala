package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding, PredicateGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.EnglishGroundingTest
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.Seq

class TestGrounding extends EnglishGroundingTest {
  // Grounding needs to be activated in englishTest.conf for these tests to be active.
  // Update: Grounding is now activated in EnglishTests by default.
  // They are usually not because of the large vector file that is required for realistic tests.
  // Furthermore, the appropriate grounder, such as wm_compositional needs to be activated.

  // This test is extremely outdated.  The query of the data structures is probably not
  // right, plus the expected values have been changed to match the actual values.
  // The test is mostly good for detecting unexpected changes.

  abstract class CompositionalGroundingTextTester {
    val groundTopN: Option[Int] = Option(5)
    val threshold: Option[Float] = Option(0.5f)
    val active: Boolean

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
                         topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold):
    (Seq[EidosMention], Seq[EidosMention])

    def allGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[Seq[String]]

    def groundingShouldContain(mention: EidosMention, value: String, slot: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      if (active) {
        val groundingNames = allGroundingNames(mention, topN, threshold)
        slot match {
          case "theme" => groundingNames.head.head should be(value)
          case "themeProperty" => groundingNames.head(1) should be(value)
          case "process" => groundingNames.head(2) should be(value)
          case "processProperty" => groundingNames.head(3) should be(value)
        }
      }
    }

    def testBranch(grounding: String, branch: String): Unit = {
      if (grounding.nonEmpty) grounding should startWith (branch) else grounding should startWith ("")
    }

    def properBranchForSlot(mention: EidosMention, slot: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      val groundingNames = allGroundingNames(mention, topN, threshold)
      slot match {
        case "theme" => testBranch(groundingNames.head.head, "wm/concept/")
        case "themeProperty" => testBranch(groundingNames.head(1), "wm/property/")
        case "process" => testBranch(groundingNames.head(2), "wm/process/")
        case "processProperty" => testBranch(groundingNames.head(3), "wm/property/")
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

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[Seq[String]] = Seq(Seq.empty)
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

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[Seq[String]] = {
      val allGroundings = groundings(mention, topN, threshold)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding =>
          val predicateGrounding = grounding.asInstanceOf[PredicateGrounding]
          val predicateTuple = predicateGrounding.predicateTuple
          val theme = predicateTuple.theme
          val theme_name = theme.grounding.headOption.map(_.name).getOrElse("")
          val property = predicateTuple.themeProperties
          val property_name = property.grounding.headOption.map(_.name).getOrElse("")
          val process = predicateTuple.themeProcess
          val process_name = process.grounding.headOption.map(_.name).getOrElse("")
          val process_property = predicateTuple.themeProcessProperties
          val process_property_name = process_property.grounding.headOption.map(_.name).getOrElse("")
          val tuple = Seq(theme_name, property_name, process_name, process_property_name)
          tuple
        }
      }
      names
    }.toSeq
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

  val theme = "theme"
  val themeProperty = "themeProperty"
  val process = "process"
  val processProperty = "processProperty"
  val slots: Seq[String] = Seq("theme", "themeProperty", "process", "processProperty")

  val tester: CompositionalGroundingTextTester = CompositionalGroundingTextTester("wm_compositional")

  {
    behavior of "Grounding 1"

    val text = "The oil supply caused civil unrest in Sudan."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 3)), Seq(Interval(4, 6)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/goods/fuel", "wm/property/availability", "", "")
    val effectGroundings = Seq("wm/process/conflict/insurgency", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      failingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      failingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "Grounding 2"

    val text = "The prices of oil caused conflict in Ethiopia."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 4)), Seq(Interval(5, 6)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/goods/fuel", "wm/property/price_or_cost", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "Grounding 3"

    val text = "Conflict caused an increase in the transportation price of fresh water."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 1)), Seq(Interval(5, 11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")
    val effectGroundings = Seq("wm/concept/goods/water/", "", "wm/process/transportation/", "wm/property/price_or_cost")

    // test cause slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "Grounding 4"

    val text = "Armed clashes caused a decrease in the supply of school supplies in Jonglei State."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/hostility", "", "", "")
    val effectGroundings = Seq("wm/concept/infrastructure/education_supplies", "wm/property/availability", "", "")

    // test cause slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      failingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "Grounding 5"

    val text = "Food security was mentioned as the main reason for flight."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(9, 10)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/goods/food", "wm/property/security", "", "")
    val effectGroundings = Seq("wm/process/population/migrate/emigrate", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  // TODO: update test for multiple causes

  //  {
  //    behavior of "Grounding 6"
  //
  //    val text = "The primary reasons for moving were insecurity, lack of food, and poor access to services such as healthcare and education."
  //    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(6, 7), Interval(8, 11), Interval(14, 22), Interval(4, 5)), Seq(Interval(4, 5)))
  //    val causeMentions = eidosMentions._1
  //    val effectMentions = eidosMentions._2
  //
  //    passingTest should "process \"" + text + "\" cause1 correctly" taggedAs Somebody in {
  //      tester.groundingShouldContain(causeMentions.head, "wm/property/insecurity")
  //    }
  //    passingTest should "process \"" + text + "\" cause2 correctly" taggedAs Somebody in {
  //      tester.groundingShouldContain(causeMentions(1), "wm/concept/goods/food")
  //    }
  //    passingTest should "process \"" + text + "\" cause3 correctly" taggedAs Somebody in {
  //      // FIXME:  'access' not grounding properly; the others work fine
  //      // TODO: It seems like the second one is the problem.  It has been commented out for regression testing.
  //      tester.groundingShouldContain(causeMentions(2), "wm/concept/economy/commercial_enterprise", topN = Option(50), threshold = Option(0.0f))
  //    }
  //    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
  //      tester.groundingShouldContain(effectMentions.head, "wm/concept/health/life")
  //    }
  //  }


  {
    behavior of "Grounding ACCESS"

    val text = "sorghum access caused an increase in migration."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 7)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/agriculture/crop/sorghum", "wm/property/availability", "wm/process/access", "")
    val effectGroundings = Seq("wm/concept/entity/people/migration/", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "Grounding SUPPLY"

    val text = "sorghum shortage caused an increase in migration."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0, 2)), Seq(Interval(6, 7)))
    println(eidosMentions)
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/agriculture/crop/sorghum", "wm/property/unavailability", "", "")
    val effectGroundings = Seq("wm/concept/entity/people/migration/", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      failingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  // TODO: in progress to address github issue
  {
    behavior of "Grounding ISSUE #739"

    val text = "The price of oil increased the price of water transportation."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,4)), Seq(Interval(5,10)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/goods/fuel", "wm/property/price_or_cost", "", "")
    val effectGroundings = Seq("wm/concept/goods/water", "", "wm/process/transportation/", "wm/property/price_or_cost")

    // test cause slots
    for (i <- slots.indices) {
      passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
      }
      failingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
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

      val causeGroundings = Seq(theme, themeProperty, process, processProperty)
      val effectGroundings = Seq(theme, themeProperty, process, processProperty)

      // test cause slots
      for (i <- slots.indices) {
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
        passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
        }
      }
      // test effect slots
      for (i <- slots.indices) {
        passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
        passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
        }
      }
    }
  */
}
