package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundingMap
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

    // TODO: Map form theme to index and branch name

    def groundingShouldContain(mention: EidosMention, value: String, slot: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      if (active) {
        val groundingNames = headGroundingNames(mention, topN, threshold)

        def compare(index: Int): Unit =
            (slot, groundingNames(index)) should be((slot, value))

        val index = slot match {
          case "theme" => 0
          case "themeProperty" => 1
          case "process" => 2
          case "processProperty" => 3
        }
        compare(index)
      }
    }

    def groundingShouldNotContain(mention: EidosMention, value: String, slot: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      val groundingNames = headGroundingNames(mention, topN, threshold)

      def compare(index: Int): Unit =
          (slot, groundingNames(index)) should not be((slot, value))

      val index = slot match {
        case "theme" => 0
        case "themeProperty" => 1
        case "process" => 2
        case "processProperty" => 3
      }
      compare(index)
    }

    def testBranch(grounding: String, branch: String): Unit = {
      if (grounding.nonEmpty) grounding should startWith (branch) else grounding should startWith ("")
    }

    def headGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[String] = {
      val groundingNames = allGroundingNames(mention, topN, threshold)
      val headGroundingNames =
          if (groundingNames.nonEmpty) groundingNames.head
          else Seq("", "", "", "")

      headGroundingNames
    }

    def properBranchForSlot(mention: EidosMention, slot: String, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      val groundingNames = headGroundingNames(mention, topN, threshold)

      slot match {
        case "theme" => testBranch(groundingNames(0), "wm/concept/")
        case "themeProperty" => testBranch(groundingNames(1), "wm/property/")
        case "process" => testBranch(groundingNames(2), "wm/process/")
        case "processProperty" => testBranch(groundingNames(3), "wm/property/")
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

    def groundings(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): OntologyGroundingMap = {
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundEidosMention(mention, topN = groundTopN, threshold = threshold)
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branchOpt.map { branch => "/" + branch }.getOrElse("")

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
        ontologyGrounding.individualGroundings.map { grounding =>
          val predicateGrounding = grounding.asInstanceOf[PredicateGrounding]
          val predicateTuple = predicateGrounding.predicateTuple
          val theme = predicateTuple.theme
          val theme_name = theme.individualGroundings.headOption.map(_.name).getOrElse("")
          val property = predicateTuple.themeProperties
          val property_name = property.individualGroundings.headOption.map(_.name).getOrElse("")
          val process = predicateTuple.themeProcess
          val process_name = process.individualGroundings.headOption.map(_.name).getOrElse("")
          val process_property = predicateTuple.themeProcessProperties
          val process_property_name = process_property.individualGroundings.headOption.map(_.name).getOrElse("")
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
        ontologyGrounding.individualGroundings.map { grounding => (grounding.name, grounding.score) }
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
    behavior of "test slots 1"

    val text = "However , in the northeast , the Boko Haram conflict has had a huge impact on agriculture because of the large-scale population displacement and the restrictions imposed on agriculture activities ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(20,23)), Seq(Interval(0,17)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    val causeGroundings = Seq("wm/concept/entity/people/", "", "wm/process/population/migrate/", "")
    val effectGroundings = Seq("wm/concept/agriculture/", "", "", "") //todo: check effect groundings

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
      failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
        tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
      }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      if (i != 1)
        passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      else
        failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
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
      if (i != 1)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
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
    val effectGroundings = Seq("", "wm/property/price_or_cost", "", "")

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "test slots 16"

    val text = "The outlook for 2020 continues to be bleak as foreign exchange reserves shrink , budget deficits increase and unemployment rates rise steeply due to the economic impacts of the pandemic ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(25,30)), Seq(Interval(18,20)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/economy/economy", "", "", "")
    val effectGroundings = Seq("wm/concept/economy/unemployment", "", "", "") //todo: add 'rate' property?

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
      if (i != 1)
        passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      else
        failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/crisis_or_disaster/environmental/", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/economy/exchange_rate", "process")
    }
  }


  {
    behavior of "test slots 17"

    val text = "The impact of research led productivity growth on poverty in Africa , Asia and Latin America ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,4)), Seq(Interval(5,16)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("", "", "wm/process/research", "")
    val effectGroundings = Seq("wm/concept/poverty", "", "", "") //fixme: bad effect span

    // test cause slots
    for (i <- slots.indices) {
      if (i != 1 && i != 3)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
        failingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      passingTest should "ground to proper branch for cause \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(causeMentions.head, slots(i))
      }
    }
    // test effect slots
    for (i <- slots.indices) {
      if (i != 2)
        passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      else
        failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/economy/economy", "theme")
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/property/productivity", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/population_demographics/population_density/population_growth", "process")
    }
  }


  {
    behavior of "test slots 18"

    val text = "Prices continued to increase unseasonably in Sudan because of deteriorating economic conditions and civil unrest ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(9,12)), Seq(Interval(0,1)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/economy/economy", "wm/property/condition", "", "")
    val effectGroundings = Seq("", "wm/property/price_or_cost", "", "")

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
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/environment/higher_temperatures", "process")
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/economy/exchange_rate", "theme")
    }
  }


  {
    behavior of "test slots 19"

    val text = "Increasing tensions and violence have raised fears that a civil war or regional fragmentation could be looming ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,2)), Seq(Interval(6,7)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "")

    // test cause slots
    for (i <- slots.indices) {
      if (i != 1)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }


  {
    behavior of "test slots 20"

    val text = "Attempts at stabilizing prices are rarely completely successful because they need to be combined with safety nets and other social protection measures to mitigate the impact of higher food prices and to help prevent violent conflicts ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(10,22)), Seq(Interval(0,4)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/safety_net", "", "", "")
    val effectGroundings = Seq("", "wm/property/price_or_cost", "wm/process/stabilization", "")

    // test cause slots
    for (i <- slots.indices) {
      if (i != 1)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/property/security", "theme")
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/intervention", "process")
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/property/price_or_cost", "theme")
    }
  }


  {
    behavior of "test slots 21"

    val text = "* Late onset of rains and long midseason dry spells led to localized household food production shortfalls ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,10)), Seq(Interval(12,17)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/environment/meteorology/precipitation", "", "wm/process/start", "")
    val effectGroundings = Seq("wm/concept/goods/food", "", "wm/process/production", "wm/property/unavailability")

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/agriculture/disease/", "process")
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/process/production", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/inequality", "process")
    }
  }


  {
    behavior of "test slots 22"

    val text = "The root causes of food insecurity in Ethiopia include structural factors such as degradation of the natural environment , population pressure that resulted in land fragmentation and land-per-capita decline , backward agricultural technology / poor performance of agricultural sector and land policy , limited opportunity for diversification of income sources , unemployment and , linked to the aforementioned , the wider economic factor of basic poverty ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(19,21)), Seq(Interval(27,29)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("", "", "", "") //todo: fill these in
    val effectGroundings = Seq("", "", "", "") //todo: fill these in

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/process/population/", "theme")
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/environment/higher_temperatures", "process")
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/population_demographics/population_density/de-population", "theme")
    }
  }


  {
    behavior of "test slots 23"

    val text = "Despite the large impact of the FFD program on growth in food consumption , results show that receipt of free food distribution causes a significant increase in perceived famine risk ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(17,22)), Seq(Interval(27,30)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/goods/food", "", "wm/process/provision", "") //todo: add process for 'receiving' ?
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/famine", "", "wm/process/perceive", "wm/property/risk") //todo: add 'perceive' as a process? 'perception' exists now as a concept.

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/economy/commercial_enterprise", "process")
    }
    failingTest should "NOT process effect theme property incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/property/risk", "themeProperty")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/perception", "process")
    }
  }


  {
    behavior of "test slots 24"

    val text = "that both import and export measures have an upward impact on world prices and ( 2 ) that exporters using export measures to stabilize domestic prices improve their welfare but negatively affect net importers ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(17,24)), Seq(Interval(28,29)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("", "", "", "") //todo: fill these in
    val effectGroundings = Seq("wm/concept/health/welfare", "", "", "")

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/process/trade/export", "theme")
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/intervention", "process")
    }
  }


  {
    behavior of "test slots 25"

    val text = "They on December 12 issued a seven-day ultimatum to Ethiopia to pull out its troops and heavy fighting began on December 20 , heightening fears of a conflict that could spread in the Horn of Africa and draw in Ethiopia 's foe , Eritrea ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(11,22)), Seq(Interval(24,28)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/armed_conflict", "", "wm/process/start", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "") //todo: need 'fear' node?

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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
        tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/entity/people/military_personnel", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/crisis_or_disaster/conflict/tension", "process")
    }
  }


    {
    behavior of "aug13_785"
    val text = "Minimal Although COVID-19 restrictions are reducing access to veterinary drugs , conflict and disease are having a more significant impact on livestock production ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(2,4)), Seq(Interval(6,14)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/health/disease/COVID", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "wm/process/access", "")
    // grounding you want it NOT to find
    val notCauseGroundings = Seq("", "", "wm/concept/regulations", "")
    val notEffectGroundings = Seq("", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }

  }

  {
    behavior of "aug13_761"
    val text = "A team from the establishment headed to Addis Ababa and the refugee camps on the Somali-Ethiopian borders to get first-hand information about the humanitarian disaster affecting thousands of Somali families who are suffering famine as a result of drought and conflict ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(38,39)), Seq(Interval(28,34)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/famine", "", "", "")
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
      if (i != 2)
        passingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      else
        failingTest should "process \"" + text + "\" effect " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(effectMentions.head, effectGroundings(i), slots(i))
        }
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/health/life", "process")
    }
  }

  {
    behavior of "aug13_753"
    val text = "It might also be linked to various problems such as soil erosion , which reduces yield , or population pressure , which increases demand for food ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(15,16)), Seq(Interval(23,26)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/process/production", "", "", "")
    val effectGroundings = Seq("wm/concept/goods/food", "", "wm/process/demand", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  {
    behavior of "aug13_705"
    val text = "But the plight of Eritrea 's people was causing growing concern this week as UNICEF , the U.N. Children 's Fund , reported that apart from those displaced by the war , another 300,000 Eritreans have been suffering from hunger and illness because of a severe drought in the Horn of Africa region ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(2,7)), Seq(Interval(9,11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/entity/people/", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/entity/people/migration/migrant", "process")
    }
  }

  {
    behavior of "aug13_676"
    val text = "The U.N. Food and Agriculture Organization appealed today for $ 32.6 million in aid for farmers in the four Horn of Africa nations and Kenya , saying millions of people there are suffering from hunger because of drought and the Eritrean-Ethiopian war ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(37,38)), Seq(Interval(30,35)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/famine", "", "", "")
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
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/health/life", "process")
    }
  }

  {
    behavior of "aug13_672"
    val text = "In Mauritania , drought is already causing serious hardship and is spreading to five neighbouring countries , affecting up to 1.5 million people ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(3,4)), Seq(Interval(7,9)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/tension", "", "", "")
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
    behavior of "aug13_660"
    val text = "Natural population growth also aggravates population pressure ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,3)), Seq(Interval(5,7)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/process/population/", "", "", "")
    val effectGroundings = Seq("wm/process/population/", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/population_demographics/population_density/population_growth", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/environment/higher_temperatures", "process")
    }
  }

  {
    behavior of "aug13_636"
    val text = "Limited cereal supplies and the lingering impact of conflict on trade and agricultural activities contributed to sorghum , maize and wheat prices being 45-90 percent higher in December 2019 than 2018 in Juba ( FAO & WFP , 2020 ) ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(5,14)), Seq(Interval(16,17)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/goods/agricultural/", "", "", "")
    val effectGroundings = Seq("wm/concept/agriculture/crop/sorghum", "", "", "")
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
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/process/training/training", "process")
    }
  }

  {
    behavior of "aug13_613"
    val text = "Poor economic and security conditions compounded by climate shocks and the longterm impact of natural disasters worsened acute food insecurity ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,16)), Seq(Interval(17,20)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/environment/climate", "", "", "")
    val effectGroundings = Seq("wm/concept/goods/food", "wm/property/insecurity", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/crisis_or_disaster/shocks", "process")
    }
  }

  {
    behavior of "aug13_586"
    val text = "The surprise and the ease with which Ethiopia attacked was sure to increase what would likely be a substantial impact to the country 's economy and morale ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,2)), Seq(Interval(13,27)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/process/communication/informing", "", "", "")
    val effectGroundings = Seq("", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/entity/locations/neighboring_country", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/economy/economy", "process")
    }
  }

  {
    behavior of "aug13_572"
    val text = "On the other hand , prices of livestock , even for cattle , have remained stable in most parts of the Region , except in Segen and lowlands of Gamo Gofa , where the impact of abnormally dry conditions weakened livestock body conditions due to the severe shortage of pasture and browse , which has led to a decline in livestock market values ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(46,50)), Seq(Interval(32,43)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("", "", "", "")
    val effectGroundings = Seq("wm/concept/agriculture/disease/livestock_disease", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/agriculture/disease/livestock_disease", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/health/weight_gain", "process")
    }
  }

  {
    behavior of "aug13_562"
    val text = "The former dispute among the two nations surfaced not only because of the famous border dispute but rather due to political and economic disagreement and tensions , according to Medhane ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(13,16)), Seq(Interval(1,3)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/entity/border", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")
    // test cause slots
    for (i <- slots.indices) {
      if (i != 2)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/crisis_or_disaster/conflict/", "process")
    }
  }

  {
    behavior of "aug13_541"
    val text = "The military strike which was Ethiopia 's first military incursion since the two countries ended the 1998-2000 border war , increased fears of a return to a full scale war ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,3)), Seq(Interval(21,30)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/process/conflict/attack", "", "", "")
    val effectGroundings = Seq("", "", "wm/process/conflict/war", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/health/malnutrition", "theme")
    }
  }

  {
    behavior of "aug13_529"
    val text = "What type of paradigms and actions in terms of leadership , people 's participation , resource mobilisation and our implementation , monitoring and evaluation strategies are required to ensure impact and rapid implementation ?"
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(15,17)), Seq(Interval(31,33)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/environment/natural_resources/", "", "", "")
    val effectGroundings = Seq("wm/concept/intervention", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/crisis_or_disaster/conflict/hostility", "process")
    }
  }

  {
    behavior of "aug13_513"
    val text = "As for the latest updates in the region and their impact in Somalia , Abdulmineim Abu Edress said the war led by Eritrea and Ethiopia on Somali soil will stop now , creating a more peaceful and secure atmosphere in the country ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(19,28)), Seq(Interval(32,42)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("", "", "", "")
    val effectGroundings = Seq("", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/entity/muslim_communities", "theme")
    }
    passingTest should "NOT process cause theme incorrectly 2" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/environment/natural_resources/soil", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/process/training/humanitarian_training/emergency_preparedness_training", "theme")
    }
  }

  {
    behavior of "aug13_501"
    val text = "The negative impact of the conflict on the economy further exacerbates the already desperate living conditions of millions of vulnerable South Sudanese ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,9)), Seq(Interval(12,22)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/economy/economy", "", "", "")
    val effectGroundings = Seq("wm/concept/health/life", "", "", "")
    // test cause slots
    for (i <- slots.indices) {
      if (i != 1)
        passingTest should "process \"" + text + "\" cause " + slots(i) + " correctly" taggedAs Somebody in {
          tester.groundingShouldContain(causeMentions.head, causeGroundings(i), slots(i))
        }
      else
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/environment/climate_change", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/environment/higher_temperatures", "process")
    }
  }

  {
    behavior of "aug13_474"
    val text = "APA - Addis Ababa ( Ethiopia ) The African Union has called on both Ethiopia and Eritrea to exercise restraint and prevent their mutual animosity to degenerate into open conflict ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(8,20)), Seq(Interval(23,25)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/discontent", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/process/conflict/torture", "theme")
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/health/weight_gain", "process")
    }
  }

  {
    behavior of "aug13_467"
    val text = "It is not possible to test whether this large impact of FFD on growth in food consumption reflects persistence of food aid received immediately after the drought because the data on FFD receipts are reported over the entire period rather than on a monthly basis ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(29,45)), Seq(Interval(18,22)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/entity/field_reports", "", "", "")
    val effectGroundings = Seq("wm/concept/goods/food", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/health/case_volume", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/humanitarian_assistance/humanitarian_assistance", "process")
    }
  }

  {
    behavior of "aug13_454"
    val text = "For Belg rain dependent areas , the food security situation for farmers and agro-pastoralists will likely deteriorate as household food stocks start depleting , while the rain will improve the pasture and water conditions ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(26,27)), Seq(Interval(30,34)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/environment/meteorology/precipitation", "", "", "")
    val effectGroundings = Seq("wm/concept/environment/natural_resources/pasture", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/environment/higher_temperatures", "process")
    }
  }

  {
    behavior of "aug13_433"
    val text = "These conflicts resulted in deaths from conflict and impact of terrorism , increasing by five and 13 per cent respectively , with a major proportion of the increase being due to the conflicts in Syria , Iraq , and Afghanistan ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,2)), Seq(Interval(4,11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  {
    behavior of "aug13_421"
    val text = "Last year , the Gambia ' s cropping season was marked by the late onset of rains and long dry spells , which resulted in a 50 percent drop in crop production compared with the five-year average ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(13,21)), Seq(Interval(26,32)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/environment/meteorology/precipitation", "", "", "")
    val effectGroundings = Seq("wm/concept/agriculture/crop/", "", "wm/process/production", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/agriculture/disease/", "process")
    }
  }

  {
    behavior of "aug13_415"
    val text = "Minimal / Moderate Overall 2020 cereal production was not notably affected by factors attributed to COVID-19 , though drought led to poor production ; 2021 production falls outside of the projection period but based on the most likely assumptions , COVID-19 is expected to have minimal to moderate impacts ., COVID - related effects will exacerbate pre-existing poor economic conditions which in 19 turn impact the capacity to invest in agricultural production ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(18,19)), Seq(Interval(21,23)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")
    val effectGroundings = Seq("wm/concept/poverty", "", "", "")
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
    behavior of "aug13_399"
    val text = "Predicting threats will allow timelier implementation of preventive and control measures , and thus will reduce their impact and limit their geographic spread ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,2)), Seq(Interval(4,11)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/process/conflict/threat", "", "wm/process/prediction", "")
    val effectGroundings = Seq("wm/concept/health/treatment/preventative_treatment", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/intervention", "process")
    }
  }

  {
    behavior of "aug13_393"
    val text = "As conflict continues to be a primary driver of poverty and suffering 5 , donors and implementers alike need to continue investing in research and evaluations that test the impact that conflict management has on economic activity and wellbeing outcomes ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(1,2)), Seq(Interval(9,10)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/conflict/", "", "", "")
    val effectGroundings = Seq("wm/concept/poverty", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  {
    behavior of "aug13_381"
    val text = "The brewing conflict had already had a serious impact in disrupting farming which had led to higher prices ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(11,12)), Seq(Interval(16,18)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/agriculture/", "", "", "")
    val effectGroundings = Seq("wm/property/price_or_cost", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
  }

  {
    behavior of "aug13_370"
    val text = "The underlying push was that creating a window for the aggrieved to release some fume of anger would invariably reduce the concentration of conflict ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(7,11)), Seq(Interval(12,17)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("", "", "", "")
    val effectGroundings = Seq("wm/concept/crisis_or_disaster/conflict/discontent", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    failingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/crisis_or_disaster/conflict/discontent", "theme")
    }
    passingTest should "NOT process cause theme incorrectly 2" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/concept/infrastructure/housing", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/environment/emissions", "process")
    }
  }

  {
    behavior of "aug13_367"
    val text = "Large scale aerial and ground control operations mitigated the impact on pastures and crops , despite the logistical and operational constraints caused by COVID 19 related restrictive measures ."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,7)), Seq(Interval(9,14)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2
    val causeGroundings = Seq("wm/concept/entity/drone", "", "", "")
    val effectGroundings = Seq("wm/concept/agriculture/crop/crops", "", "", "")
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
      passingTest should "ground to proper branch for effect \"" + slots(i) + "\" slot" taggedAs Somebody in {
      tester.properBranchForSlot(effectMentions.head, slots(i))
      }
    }
    passingTest should "NOT process cause theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(causeMentions.head, "wm/process/training/agriculture_training", "process")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/economy/economy", "process")
    }
  }

  {
    behavior of "test slots scores"

    val text = "Drought caused population growth."
    val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(0,1)), Seq(Interval(2,4)))
    val causeMentions = eidosMentions._1
    val effectMentions = eidosMentions._2

    // order is:  theme, theme property, process, process property
    val causeGroundings = Seq("wm/concept/crisis_or_disaster/environmental/drought", "", "", "")
    val effectGroundings = Seq("wm/concept/population_demographics/population_density/population_growth", "", "", "")

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
    passingTest should "NOT process effect theme incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/concept/population_demographics/", "theme")
    }
    passingTest should "NOT process effect process incorrectly" taggedAs Somebody in {
      tester.groundingShouldNotContain(effectMentions.head, "wm/process/development", "process")
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
