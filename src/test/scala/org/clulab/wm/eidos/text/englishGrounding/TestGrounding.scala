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

  // BEFORE new unit tests: 76 passed, 52 ignored
  // Start of new unit tests 08/24/2021

  {
    behavior of "test slots 1"

    val text = "However , in the northeast , the Boko Haram conflict has had a huge impact on agriculture because of the large-scale population displacement and the restrictions imposed on agriculture activities ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(20,23)), Seq(Interval(0,17)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/entity/people/", "", "wm/process/population/migrate/", "")
    val effectGroundings = Seq("wm/concept/agriculture/", "", "", "") //todo: check effect groundings

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
    behavior of "test slots 2"

    val text = "With the forecast conclusion of the March - June seasonal rains across much of the eastern Horn in June , rangeland conditions ( vegetation and surface water ) are expected to gradually decline due to the poor performance of the long-rains season and dry conditions forecast into late October ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(36,45)), Seq(Interval(20,22)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/time/wet_season", "", "", "")
    val effectGroundings = Seq("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", "")

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
    behavior of "test slots 3"

    val text = "In most southern and southeastern pastoral areas , the below-average October to December deyr / hagaya season and persistent desert locust swarms also led to below-normal vegetation conditions ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(15,17)), Seq(Interval(25,28)))
    //todo: add example "deyr / hagaya season" to wet_season node
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/time/wet_season", "", "", "")
    val effectGroundings = Seq("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", "")

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
    behavior of "test slots 4"

    val text = "Increased food insecurity and malnutrition is likely to decrease human disease resistance and human labour productivity and increase human deaths , unless health services , which are currently very poor in these areas , are improved in the coming years ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,3)), Seq(Interval(9,12)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/goods/food", "wm/property/insecurity", "", "")
    val effectGroundings = Seq("wm/concept/health/disease/", "", "", "") //todo: add nodes for disease resistance?

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
    behavior of "test slots 5"

    val text = "Ethiopia 's cancer control strategy which mainly focuses on wide-range of prevention policy and strategy supported by the recent strict measures will help to reduce the impact of cancer in the country , he said ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,6)), Seq(Interval(26,33)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/plan/", "", "", "") //todo: or "intervention"?
    val effectGroundings = Seq("wm/concept/health/disease/illness", "", "", "") //todo: "aftermath" as property?

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
    behavior of "test slots 6"

    val text = "The impact of the drought has been exacerbated by high local cereal prices , excess livestock mortality , conflict and restricted humanitarian access in some areas ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(14,17)), Seq(Interval(1,5)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/agriculture/livestock_nonpoultry", "", "wm/process/death", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")

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
    behavior of "test slots 7"

    val text = "The cumulative impact of two consecutive below-average rainy seasons has resulted in widespread poor vegetation conditions , severely affecting crop growth and pasture availability ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,9)), Seq(Interval(12,16)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/time/wet_season", "", "", "")
    val effectGroundings = Seq("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", "")
    //todo: new node for "vegetation" that isn't "pasture" or "forestry"?

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
    behavior of "test slots 8"

    val text = "Prices of all staple foods are above-average ( Figure 7 ) due to the deteriorating economic conditions and are expected to remain elevated in the coming months ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(14,17)), Seq(Interval(0,5)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/economy/economy", "", "", "")
    val effectGroundings = Seq("wm/concept/goods/food", "wm/property/price_or_cost", "", "")

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
    behavior of "test slots 9"

    val text = "President Girma Woldegiorgis and Senior Ethiopian government officials sent messages of condolence to their Indian counterparts over the recent tragedy in Mumbai , India due to terrorist attacks ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(26,28)), Seq(Interval(0,22)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/crime", "", "wm/process/conflict/terrorism", "") //todo: need process as well?
    val effectGroundings = Seq("wm/concept/government/", "", "wm/process/communication/informing", "") //todo: needs better process, probably

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
    behavior of "test slots 10"

    val text = "As of the first quarter of 2011 the Djiboutian government remains genuinely worried that a potential Afar insurgency in the north could quickly spread to the south , especially in view of the fact that the Djiboutian National Army is weak and the population in Djibouti City is facing deteriorating economic conditions due to high unemployment and inflation , which surged to 3,8 per cent in 2010 ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(54,56)), Seq(Interval(49,52)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/economy/unemployment", "", "", "")
    val effectGroundings = Seq("wm/concept/economy/economy", "wm/property/condition", "", "")
    //todo: need process for 'deteriorate' ?

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
    behavior of "test slots 11"

    val text = "Future work should focus on the implementation of control measures that mitigate the economic impact of the disease ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(6,10)), Seq(Interval(13,18)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/regulations", "", "", "")
    val effectGroundings = Seq("wm/concept/economy/economy", "", "", "")

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
    behavior of "test slots 12"

    val text = "The brewing conflict had already had a serious impact in disrupting farming which had led to higher prices ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(11,12)), Seq(Interval(16,18)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/agriculture/", "", "", "")
    val effectGroundings = Seq("", "wm/property/price_or_cost", "", "") //fixme: what about plain 'prices'? still need a theme?

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
    behavior of "test slots 13"

    val text = "Tensions run high between the two countries with a total of 200,000 troops from both sides facing off on either side of their border , threatening a fresh conflict ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,1)), Seq(Interval(27,29)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "")
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
    behavior of "test slots 14"

    val text = "When the next drought comes , it will definitely have an impact on us because the cost of feeding our donkeys will go up , and people will no longer hire us to transport grasses , but that 's it , says Barni ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(16,24)), Seq(Interval(0,12)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    //fixme: bad causal extractions in general

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/agriculture/livestock_nonpoultry", "", "", "wm/property/price_or_cost") //fixme: needs process for 'feeding'
    val effectGroundings = Seq("", "", "", "") //todo: effect is really generic 'impact'

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
    behavior of "test slots 15"

    val text = "Hence , in circumstances where property rights and conflict management institutions are ineffective or illegitimate , efforts to mitigate or adapt to climate change that change the distribution of access to resources have the potential to create and aggravate conflict ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(16,24)), Seq(Interval(39,40)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/environment/climate_change", "", "wm/process/mitigation", "") //fixme: grounding currently just 'climate'
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

      // order is:  theme, theme property, process, process property
      val causeGroundings = Seq("", "", "", "")
      val effectGroundings = Seq("", "", "", "")

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
