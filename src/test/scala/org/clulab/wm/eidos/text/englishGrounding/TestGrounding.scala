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

  val CONCEPT_BRANCH = "concept"
  val PROCESS_BRANCH = "process"
  val PROPERTY_BRANCH = "property"

  val THEME_SLOT = 0
  val THEME_PROPERTY_SLOT = 1
  val PROCESS_SLOT = 2
  val PROCESS_PROPERTY_SLOT = 3

  type Groundings = Tuple4[String, String, String, String]
  type GroundingExtractor = Groundings => String

  type Modes = Tuple4[Int, Int, Int, Int]
  type ModeExtractor = Modes => Int

  case class Slot(index: Int, name: String, prefix: String, groundingExtractor: GroundingExtractor, modeExtractor: ModeExtractor)

  val slots: Array[Slot] = Array(
    Slot(0, "theme",           s"wm/$CONCEPT_BRANCH/",  tuple => tuple._1, tuple => tuple._1),
    Slot(1, "themeProperty",   s"wm/$PROPERTY_BRANCH/", tuple => tuple._2, tuple => tuple._2),
    Slot(2, "process",         s"wm/$PROCESS_BRANCH/",  tuple => tuple._3, tuple => tuple._3),
    Slot(3, "processProperty", s"wm/$PROPERTY_BRANCH/", tuple => tuple._4, tuple => tuple._4)
  )

  val FAIL = 0
  val PASS = 1
  val IGNORE = 2

  val CAUSE = "cause"
  val EFFECT = "effect"

  abstract class CompositionalGroundingTextTester {
    val groundTopN: Option[Int] = Option(5)
    val threshold: Option[Float] = Option(0.5f)
    val active: Boolean

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
        topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): (Seq[EidosMention], Seq[EidosMention])

    def allGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[Seq[String]]

    // TODO: Map form theme to index and branch name

    def groundingShouldContain(mention: EidosMention, groundings: Groundings, slot: Slot, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      if (active) {
        val groundingNames = headGroundingNames(mention, topN, threshold)
        val slotName = slot.name

        (slotName, groundingNames(slot.index)) should be((slotName, slot.groundingExtractor(groundings)))
      }
    }

    def groundingShouldNotContain(mention: EidosMention, value: String, slot: Int, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      val groundingNames = headGroundingNames(mention, topN, threshold)
      val slotName = slots(slot).name

      (slotName, groundingNames(slot)) should not be((slotName, value))
    }

    def headGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[String] = {
      val groundingNames = allGroundingNames(mention, topN, threshold)
      val headGroundingNames =
          if (groundingNames.nonEmpty) groundingNames.head
          else Seq("", "", "", "")

      headGroundingNames
    }

    def properBranchForSlot(mention: EidosMention, slot: Slot, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Unit = {
      val grounding = headGroundingNames(mention, topN, threshold)(slot.index)

      if (grounding.nonEmpty) grounding should startWith (slot.prefix)
      else grounding should startWith ("")
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
    def topConceptGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, CONCEPT_BRANCH)

    def topPropertyGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, PROPERTY_BRANCH)

    def topProcessGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, PROCESS_BRANCH)

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

  val tester: CompositionalGroundingTextTester = CompositionalGroundingTextTester("wm_compositional")

  case class Test(
      name: String,
      text: String,

      causeInterval: Interval,
      causeGroundings: Groundings,
      causeModes: Modes,

      effectInterval: Interval,
      effectGroundings: Groundings,
      effectModes: Modes,

      causeNotGroundings: Seq[(String, Int)] = Seq.empty,
      effectNotGroundings: Seq[(String, Int)] = Seq.empty
  ) {

    def test(typ: String, groundings: Groundings, modes: Modes, mentions: Seq[EidosMention]): Unit = {
      slots.foreach { slot =>
        val title = s"""process "$text" $typ ${slot.name} correctly"""

        slot.modeExtractor(modes) match {
          case FAIL =>
            failingTest should title taggedAs Somebody in {
              tester.groundingShouldContain(mentions.head, groundings, slot)
            }
          case PASS =>
            passingTest should title taggedAs Somebody in {
              tester.groundingShouldContain(mentions.head, groundings, slot)
            }
          case IGNORE =>
            ignore should title taggedAs Somebody in {
              tester.groundingShouldContain(mentions.head, groundings, slot)
            }
        }
      }

      slots.foreach { slot =>
        val title = s"""ground to proper branch for $typ "${slot.name}" slot"""

        passingTest should title taggedAs Somebody in {
          tester.properBranchForSlot(mentions.head, slot)
        }
      }
    }

    def testNot(typ: String, mentions: Seq[EidosMention], notGroundings: Seq[(String, Int)]): Unit = {
      notGroundings.foreach { case (node, slot) =>
        passingTest should s"NOT process $typ ${slots(slot).name} incorrectly" taggedAs Somebody in {
          tester.groundingShouldNotContain(mentions.head, node, slot)
        }
      }
    }

    def test(): Unit = {
      behavior of name

      val (causeMentions, effectMentions) = tester.fakeAnnotatedDoc(text, Seq(causeInterval), Seq(effectInterval))

      test(CAUSE, causeGroundings, causeModes, causeMentions)
      test(EFFECT, effectGroundings, effectModes, effectMentions)

      testNot(CAUSE, causeMentions, causeNotGroundings)
      testNot(EFFECT, effectMentions, effectNotGroundings)
    }
  }

  val tests: Array[Test] = Array(
    Test(
      "test slots 1",
      "However , in the northeast , the Boko Haram conflict has had a huge impact on agriculture because of the large-scale population displacement and the restrictions imposed on agriculture activities .",
      Interval(20, 23),
      ("wm/concept/entity/people/", "", "wm/process/population/migrate/", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(0, 17),
      ("wm/concept/agriculture/", "", "", ""), //todo: check effect groundings
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 2",
      "With the forecast conclusion of the March - June seasonal rains across much of the eastern Horn in June , rangeland conditions ( vegetation and surface water ) are expected to gradually decline due to the poor performance of the long-rains season and dry conditions forecast into late October .",
      Interval(36, 45),
      ("wm/concept/time/wet_season", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(20, 22),
      ("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    //todo: add example "deyr / hagaya season" to wet_season node
    Test(
      "test slots 3",
      "In most southern and southeastern pastoral areas , the below-average October to December deyr / hagaya season and persistent desert locust swarms also led to below-normal vegetation conditions .",
      Interval(15, 17),
      ("wm/concept/time/wet_season", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(25, 28),
      ("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 4",
      "Increased food insecurity and malnutrition is likely to decrease human disease resistance and human labour productivity and increase human deaths , unless health services , which are currently very poor in these areas , are improved in the coming years .",
      Interval(0, 3),
      ("wm/concept/goods/food", "wm/property/insecurity", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(9, 12),
      ("wm/concept/health/disease/", "", "", ""), //todo: add nodes for disease resistance?
      (FAIL, FAIL, FAIL, FAIL),
    ),
    Test(
      "test slots 5",
      "Ethiopia 's cancer control strategy which mainly focuses on wide-range of prevention policy and strategy supported by the recent strict measures will help to reduce the impact of cancer in the country , he said .",
      Interval(0, 6),
      ("wm/concept/plan/", "", "", ""), //todo: or "intervention"?
      (FAIL, FAIL, FAIL, FAIL),
      Interval(26, 33),
      ("wm/concept/health/disease/illness", "", "", ""), //todo: "aftermath" as property?
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 6",
      "The impact of the drought has been exacerbated by high local cereal prices , excess livestock mortality , conflict and restricted humanitarian access in some areas .",
      Interval(14, 17),
      ("wm/concept/agriculture/livestock_nonpoultry", "", "wm/process/death", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(1, 5),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, FAIL, PASS, PASS)
    ),
    Test(
      "test slots 7",
      "The cumulative impact of two consecutive below-average rainy seasons has resulted in widespread poor vegetation conditions , severely affecting crop growth and pasture availability .",
      Interval(1, 9),
      ("wm/concept/time/wet_season", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(12, 16),
      ("wm/concept/environment/natural_resources/pasture", "wm/property/condition", "", ""),
      //todo: new node for "vegetation" that isn't "pasture" or "forestry"?
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 8",
      "Prices of all staple foods are above-average ( Figure 7 ) due to the deteriorating economic conditions and are expected to remain elevated in the coming months .",
      Interval(14, 17),
      ("wm/concept/economy/economy", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(0,5),
      ("wm/concept/goods/food", "wm/property/price_or_cost", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 9",
      "President Girma Woldegiorgis and Senior Ethiopian government officials sent messages of condolence to their Indian counterparts over the recent tragedy in Mumbai , India due to terrorist attacks .",
      Interval(26, 28),
      ("wm/concept/crisis_or_disaster/conflict/crime", "", "wm/process/conflict/terrorism", ""), //todo: need process as well?
      (FAIL, FAIL, FAIL, FAIL),
      Interval(0, 22),
      ("wm/concept/government/", "", "wm/process/communication/informing", ""), //todo: needs better process, probably
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 10",
      "As of the first quarter of 2011 the Djiboutian government remains genuinely worried that a potential Afar insurgency in the north could quickly spread to the south , especially in view of the fact that the Djiboutian National Army is weak and the population in Djibouti City is facing deteriorating economic conditions due to high unemployment and inflation , which surged to 3,8 per cent in 2010 .",
      Interval(54, 56),
      ("wm/concept/economy/unemployment", "", "", ""),
      (PASS, FAIL, PASS, PASS),
      Interval(49, 52),
      ("wm/concept/economy/economy", "wm/property/condition", "", ""),
      //todo: need process for 'deteriorate' ?
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 11",
      "Future work should focus on the implementation of control measures that mitigate the economic impact of the disease .",
      Interval(6, 10),
      ("wm/concept/regulations", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(13, 18),
      ("wm/concept/economy/economy", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 12",
      "The brewing conflict had already had a serious impact in disrupting farming which had led to higher prices .",
      Interval(11, 12),
      ("wm/concept/agriculture/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(16, 18),
      ("", "wm/property/price_or_cost", "", ""),
      (PASS, FAIL, PASS, PASS)
    ),
    Test(
      "test slots 13",
      "Tensions run high between the two countries with a total of 200,000 troops from both sides facing off on either side of their border , threatening a fresh conflict .",
      Interval(0, 1),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(27, 29),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 14",
      "When the next drought comes , it will definitely have an impact on us because the cost of feeding our donkeys will go up , and people will no longer hire us to transport grasses , but that 's it , says Barni .",
      Interval(16, 24),
      //fixme: bad causal extractions in general
      ("wm/concept/agriculture/livestock_nonpoultry", "", "", "wm/property/price_or_cost"), //fixme: needs process for 'feeding'
      (FAIL, FAIL, FAIL, FAIL),
      Interval(0, 12),
      ("", "", "", ""), //todo: effect is really generic 'impact'
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 15",
      "Hence , in circumstances where property rights and conflict management institutions are ineffective or illegitimate , efforts to mitigate or adapt to climate change that change the distribution of access to resources have the potential to create and aggravate conflict .",
      Interval(16, 24),
      ("wm/concept/environment/climate_change", "", "wm/process/mitigation", ""), //fixme: grounding currently just 'climate'
      (FAIL, FAIL, FAIL, FAIL),
      Interval(39, 40),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 16",
      "The outlook for 2020 continues to be bleak as foreign exchange reserves shrink , budget deficits increase and unemployment rates rise steeply due to the economic impacts of the pandemic .",
      Interval(25, 30),
      ("wm/concept/economy/economy", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(18, 20),
      ("wm/concept/economy/unemployment", "", "", ""), //todo: add 'rate' property?
      (PASS, FAIL, PASS, PASS),
      Seq(("wm/concept/crisis_or_disaster/environmental/", PROCESS_SLOT)),
      Seq(("wm/concept/economy/exchange_rate", PROCESS_SLOT))
    ),
    Test(
      "test slots 17",
      "The impact of research led productivity growth on poverty in Africa , Asia and Latin America .",
      Interval(1, 4),
      ("", "", "wm/process/research", ""),
      (PASS, FAIL, PASS, FAIL),
      Interval(5, 16),
      ("wm/concept/poverty", "", "", ""), //fixme: bad effect span
      (PASS, PASS, FAIL, PASS),
      Seq(("wm/concept/economy/economy", THEME_SLOT)),
      Seq(
        ("wm/property/productivity", THEME_SLOT),
        ("wm/concept/population_demographics/population_density/population_growth", PROCESS_SLOT)
      )
    ),
    Test(
      "test slots 18",
      "Prices continued to increase unseasonably in Sudan because of deteriorating economic conditions and civil unrest .",
      Interval(9, 12),
      ("wm/concept/economy/economy", "wm/property/condition", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(0, 1),
      ("", "wm/property/price_or_cost", "", ""),
      (PASS, PASS, PASS, PASS),
      Seq(("wm/concept/environment/higher_temperatures", PROCESS_SLOT)),
      Seq(("wm/concept/economy/exchange_rate", THEME_SLOT))
    ),
    Test(
      "test slots 19",
      "Increasing tensions and violence have raised fears that a civil war or regional fragmentation could be looming .",
      Interval(0, 2),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""),
      (PASS, FAIL, FAIL, PASS),
      Interval(6, 7),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "test slots 20",
      "Attempts at stabilizing prices are rarely completely successful because they need to be combined with safety nets and other social protection measures to mitigate the impact of higher food prices and to help prevent violent conflicts .",
      Interval(10, 22),
      ("wm/concept/safety_net", "", "", ""),
      (PASS, FAIL, FAIL, PASS),
      Interval(0, 4),
      ("", "wm/property/price_or_cost", "wm/process/stabilization", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/property/security", THEME_SLOT),
        ("wm/concept/intervention", PROCESS_SLOT)
      ),
      Seq(("wm/property/price_or_cost", THEME_SLOT))
    ),
    Test(
      "test slots 21",
      "* Late onset of rains and long midseason dry spells led to localized household food production shortfalls .",
      Interval(1, 10),
      ("wm/concept/environment/meteorology/precipitation", "", "wm/process/start", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(12, 17),
      ("wm/concept/goods/food", "", "wm/process/production", "wm/property/unavailability"),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/agriculture/disease/", PROCESS_SLOT)),
      Seq(
        ("wm/process/production", THEME_SLOT),
        ("wm/concept/inequality", PROCESS_SLOT)
      )
    ),
    Test(
      "test slots 22",
      "The root causes of food insecurity in Ethiopia include structural factors such as degradation of the natural environment , population pressure that resulted in land fragmentation and land-per-capita decline , backward agricultural technology / poor performance of agricultural sector and land policy , limited opportunity for diversification of income sources , unemployment and , linked to the aforementioned , the wider economic factor of basic poverty .",
      Interval(19, 21),
      ("", "", "", ""), //todo: fill these in
      (FAIL, FAIL, FAIL, FAIL),
      Interval(27, 29),
      ("", "", "", ""), //todo: fill these in
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/process/population/", THEME_SLOT),
        ("wm/concept/environment/higher_temperatures", PROCESS_SLOT)
      ),
      Seq(("wm/concept/population_demographics/population_density/de-population", THEME_SLOT))

    ),
    Test(
      "test slots 23",
      "Despite the large impact of the FFD program on growth in food consumption , results show that receipt of free food distribution causes a significant increase in perceived famine risk .",
      Interval(17, 22),
      ("wm/concept/goods/food", "", "wm/process/provision", ""), //todo: add process for 'receiving' ?
      (FAIL, FAIL, FAIL, FAIL),
      Interval(27, 30),
      ("wm/concept/crisis_or_disaster/famine", "", "wm/process/perceive", "wm/property/risk"), //todo: add 'perceive' as a process? 'perception' exists now as a concept.
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/economy/commercial_enterprise", PROCESS_SLOT)),
      Seq(
        ("wm/property/risk", THEME_PROPERTY_SLOT),
        ("wm/concept/perception", PROCESS_SLOT)
      )
    ),
    Test(
      "test slots 24",
      "that both import and export measures have an upward impact on world prices and ( 2 ) that exporters using export measures to stabilize domestic prices improve their welfare but negatively affect net importers .",
      Interval(17, 24),
      ("", "", "", ""), //todo: fill these in
      (FAIL, FAIL, FAIL, FAIL),
      Interval(28, 29),
      ("wm/concept/health/welfare", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/process/trade/export", THEME_SLOT),
        ("wm/concept/intervention", PROCESS_SLOT)
      )
    ),
    Test(
      "test slots 25",
      "They on December 12 issued a seven-day ultimatum to Ethiopia to pull out its troops and heavy fighting began on December 20 , heightening fears of a conflict that could spread in the Horn of Africa and draw in Ethiopia 's foe , Eritrea .",
      Interval(11, 22),
      ("wm/concept/crisis_or_disaster/conflict/armed_conflict", "", "wm/process/start", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(24, 28),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""), //todo: need 'fear' node?
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/entity/people/military_personnel", THEME_SLOT)),
      Seq(("wm/concept/crisis_or_disaster/conflict/tension", PROCESS_SLOT))
    ),
    Test(
      "aug13_785",
      "Minimal Although COVID-19 restrictions are reducing access to veterinary drugs , conflict and disease are having a more significant impact on livestock production .",
      Interval(2, 4),
      ("wm/concept/health/disease/COVID", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(6, 14),
      ("wm/concept/crisis_or_disaster/conflict/", "", "wm/process/access", ""),
      (FAIL, FAIL, FAIL, FAIL)
      // These were never tested.
      // val notCauseGroundings = Seq("", "", "wm/concept/regulations", "")
      // val notEffectGroundings = Seq("", "", "", "")
    ),
    Test(
      "aug13_761",
      "A team from the establishment headed to Addis Ababa and the refugee camps on the Somali-Ethiopian borders to get first-hand information about the humanitarian disaster affecting thousands of Somali families who are suffering famine as a result of drought and conflict .",
      Interval(38, 39),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, PASS, PASS, PASS),
      Interval(28, 34),
      ("wm/concept/crisis_or_disaster/famine", "", "", ""),
      (PASS, PASS, FAIL, PASS),
      Seq.empty,
      Seq(("wm/concept/health/life", PROCESS_SLOT))
    ),
    Test(
      "aug13_753",
      "It might also be linked to various problems such as soil erosion , which reduces yield , or population pressure , which increases demand for food .",
      Interval(15, 16),
      ("wm/process/production", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(23, 26),
      ("wm/concept/goods/food", "", "wm/process/demand", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
       "aug13_705",
      "But the plight of Eritrea 's people was causing growing concern this week as UNICEF , the U.N. Children 's Fund , reported that apart from those displaced by the war , another 300,000 Eritreans have been suffering from hunger and illness because of a severe drought in the Horn of Africa region .",
      Interval(2, 7),
      ("wm/concept/entity/people/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(9,11),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/entity/people/migration/migrant", PROCESS_SLOT))
    ),
    Test(
      "aug13_676",
      "The U.N. Food and Agriculture Organization appealed today for $ 32.6 million in aid for farmers in the four Horn of Africa nations and Kenya , saying millions of people there are suffering from hunger because of drought and the Eritrean-Ethiopian war .",
      Interval(37, 38),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, PASS, PASS, PASS),
      
      Interval(30, 35),
      ("wm/concept/crisis_or_disaster/famine", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq.empty,
      Seq(("wm/concept/health/life", PROCESS_SLOT))
    ),
    Test(
      "aug13_672",
      "In Mauritania , drought is already causing serious hardship and is spreading to five neighbouring countries , affecting up to 1.5 million people .",
      Interval(3, 4),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, PASS, PASS, PASS),
      
      Interval(7, 9),
      ("wm/concept/crisis_or_disaster/conflict/tension", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "aug13_660",
      "Natural population growth also aggravates population pressure .",
      Interval(0, 3),
      ("wm/process/population/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(5, 7),
      ("wm/process/population/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/population_demographics/population_density/population_growth", PROCESS_SLOT)),
      Seq(("wm/concept/environment/higher_temperatures", PROCESS_SLOT))
    ),
    Test(
      "aug13_636",
      "Limited cereal supplies and the lingering impact of conflict on trade and agricultural activities contributed to sorghum , maize and wheat prices being 45-90 percent higher in December 2019 than 2018 in Juba ( FAO & WFP , 2020 ) .",
      Interval(5, 14),
      ("wm/concept/goods/agricultural/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(16, 17),
      ("wm/concept/agriculture/crop/sorghum", "", "", ""),
      (PASS, PASS, PASS, PASS),
      Seq(("wm/process/training/training", PROCESS_SLOT))
    ),
    Test(
      "aug13_613",
      "Poor economic and security conditions compounded by climate shocks and the longterm impact of natural disasters worsened acute food insecurity .",
      Interval(0, 16),
      ("wm/concept/environment/climate", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(17, 20),
      ("wm/concept/goods/food", "wm/property/insecurity", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/crisis_or_disaster/shocks", PROCESS_SLOT))
    ),
    Test(
      "aug13_586",
      "The surprise and the ease with which Ethiopia attacked was sure to increase what would likely be a substantial impact to the country 's economy and morale .",
      Interval(1, 2),
      ("wm/process/communication/informing", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(13, 27),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq.empty,
      Seq(
        ("wm/concept/entity/locations/neighboring_country", THEME_SLOT),
        ("wm/concept/economy/economy", PROCESS_SLOT)
      )
    ),
    Test(
      "aug13_572",
      "On the other hand , prices of livestock , even for cattle , have remained stable in most parts of the Region , except in Segen and lowlands of Gamo Gofa , where the impact of abnormally dry conditions weakened livestock body conditions due to the severe shortage of pasture and browse , which has led to a decline in livestock market values .",
      Interval(46, 50),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(32, 43),
      ("wm/concept/agriculture/disease/livestock_disease", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/agriculture/disease/livestock_disease", THEME_SLOT)),
      Seq(("wm/concept/health/weight_gain", PROCESS_SLOT))
    ),
    Test(
      "aug13_562",
      "The former dispute among the two nations surfaced not only because of the famous border dispute but rather due to political and economic disagreement and tensions , according to Medhane .",
      Interval(13, 16),
      ("wm/concept/entity/border", "", "", ""),
      (PASS, PASS, FAIL, PASS),
      Interval(1, 3),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/crisis_or_disaster/conflict/", PROCESS_SLOT))
    ),
    Test(
      "aug13_541",
      "The military strike which was Ethiopia 's first military incursion since the two countries ended the 1998-2000 border war , increased fears of a return to a full scale war .",
      Interval(1, 3),
      ("wm/process/conflict/attack", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(21, 30),
      ("", "", "wm/process/conflict/war", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq.empty,
      Seq(("wm/concept/health/malnutrition", THEME_SLOT))
    ),
    Test(
      "aug13_529",
      "What type of paradigms and actions in terms of leadership , people 's participation , resource mobilisation and our implementation , monitoring and evaluation strategies are required to ensure impact and rapid implementation ?",
      Interval(15, 17),
      ("wm/concept/environment/natural_resources/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(31, 33),
      ("wm/concept/intervention", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/crisis_or_disaster/conflict/hostility", PROCESS_SLOT))
    ),
    Test(
      "aug13_513",
      "As for the latest updates in the region and their impact in Somalia , Abdulmineim Abu Edress said the war led by Eritrea and Ethiopia on Somali soil will stop now , creating a more peaceful and secure atmosphere in the country .",
      Interval(19, 28),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(32, 42),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/concept/entity/muslim_communities", THEME_SLOT),
        ("wm/concept/environment/natural_resources/soil", PROCESS_SLOT)
      ),
      Seq(("wm/process/training/humanitarian_training/emergency_preparedness_training", THEME_SLOT))
    ),
    Test(
      "aug13_501",
      "The negative impact of the conflict on the economy further exacerbates the already desperate living conditions of millions of vulnerable South Sudanese .",
      Interval(1, 9),
      ("wm/concept/economy/economy", "", "", ""),
      (FAIL, FAIL, FAIL, PASS),
      
      Interval(12, 22),
      ("wm/concept/health/life", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/environment/climate_change", PROCESS_SLOT)),
      Seq(("wm/concept/environment/higher_temperatures", PROCESS_SLOT))
    ),
    Test(
      "aug13_474",
      "APA - Addis Ababa ( Ethiopia ) The African Union has called on both Ethiopia and Eritrea to exercise restraint and prevent their mutual animosity to degenerate into open conflict .",
      Interval(8, 20),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(23, 25),
      ("wm/concept/crisis_or_disaster/conflict/discontent", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/process/conflict/torture", THEME_SLOT),
        ("wm/concept/health/weight_gain", PROCESS_SLOT)
      )
    ),
    Test(
      "aug13_467",
      "It is not possible to test whether this large impact of FFD on growth in food consumption reflects persistence of food aid received immediately after the drought because the data on FFD receipts are reported over the entire period rather than on a monthly basis .",
      Interval(29, 45),
      ("wm/concept/entity/field_reports", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(18, 22),
      ("wm/concept/goods/food", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/health/case_volume", PROCESS_SLOT)),
      Seq(("wm/concept/humanitarian_assistance/humanitarian_assistance", PROCESS_SLOT))
    ),
    Test(
      "aug13_454",
      "For Belg rain dependent areas , the food security situation for farmers and agro-pastoralists will likely deteriorate as household food stocks start depleting , while the rain will improve the pasture and water conditions .",
      Interval(26, 27),
      ("wm/concept/environment/meteorology/precipitation", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(30, 34),
      ("wm/concept/environment/natural_resources/pasture", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq.empty,
      Seq(("wm/concept/environment/higher_temperatures", PROCESS_SLOT))
    ),
    Test(
      "aug13_433",
      "These conflicts resulted in deaths from conflict and impact of terrorism , increasing by five and 13 per cent respectively , with a major proportion of the increase being due to the conflicts in Syria , Iraq , and Afghanistan .",
   
      Interval(1, 2),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(4, 11),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "aug13_421",
      "Last year , the Gambia ' s cropping season was marked by the late onset of rains and long dry spells , which resulted in a 50 percent drop in crop production compared with the five-year average .",
      Interval(13, 21),
      ("wm/concept/environment/meteorology/precipitation", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(26, 32),
      ("wm/concept/agriculture/crop/", "", "wm/process/production", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/concept/agriculture/disease/", PROCESS_SLOT))
    ),
    Test(
      "aug13_415",
      "Minimal / Moderate Overall 2020 cereal production was not notably affected by factors attributed to COVID-19 , though drought led to poor production ; 2021 production falls outside of the projection period but based on the most likely assumptions , COVID-19 is expected to have minimal to moderate impacts ., COVID - related effects will exacerbate pre-existing poor economic conditions which in 19 turn impact the capacity to invest in agricultural production .",
      
      Interval(18, 19),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, PASS, PASS, PASS),
      
      Interval(21, 23),
      ("wm/concept/poverty", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "aug13_399",
      "Predicting threats will allow timelier implementation of preventive and control measures , and thus will reduce their impact and limit their geographic spread .",
      Interval(0, 2),
      ("wm/process/conflict/threat", "", "wm/process/prediction", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(4, 11),
      ("wm/concept/health/treatment/preventative_treatment", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq.empty,
      Seq(("wm/concept/intervention", PROCESS_SLOT))
    ),
    Test(
      "aug13_393",
      "As conflict continues to be a primary driver of poverty and suffering 5 , donors and implementers alike need to continue investing in research and evaluations that test the impact that conflict management has on economic activity and wellbeing outcomes .",
      Interval(1, 2),
      ("wm/concept/crisis_or_disaster/conflict/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(9, 10),
      ("wm/concept/poverty", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "aug13_381",
      "The brewing conflict had already had a serious impact in disrupting farming which had led to higher prices .",
      Interval(11, 12),
      ("wm/concept/agriculture/", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      
      Interval(16, 18),
      ("wm/property/price_or_cost", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL)
    ),
    Test(
      "aug13_370",
      "The underlying push was that creating a window for the aggrieved to release some fume of anger would invariably reduce the concentration of conflict .",
      Interval(7, 11),
      ("", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(12, 17),
      ("wm/concept/crisis_or_disaster/conflict/discontent", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(
        ("wm/concept/crisis_or_disaster/conflict/discontent", THEME_SLOT),
        ("wm/concept/infrastructure/housing", PROCESS_SLOT)
      ),
      Seq(("wm/concept/environment/emissions", PROCESS_SLOT))
    ),
    Test(
      "aug13_367",
      "Large scale aerial and ground control operations mitigated the impact on pastures and crops , despite the logistical and operational constraints caused by COVID 19 related restrictive measures .",
      Interval(0, 7),
      ("wm/concept/entity/drone", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Interval(9, 14),
      ("wm/concept/agriculture/crop/crops", "", "", ""),
      (FAIL, FAIL, FAIL, FAIL),
      Seq(("wm/process/training/agriculture_training", PROCESS_SLOT)),
      Seq(("wm/concept/economy/economy", PROCESS_SLOT))
    ),
    Test(
      "test slots scores",
      "Drought caused population growth.",
      Interval(0,1),
      ("wm/concept/crisis_or_disaster/environmental/drought", "", "", ""),
      (PASS, PASS, PASS, PASS),
      
      Interval(2,4),
      ("wm/concept/population_demographics/population_density/population_growth", "", "", ""),
      (PASS, PASS, PASS, PASS),
      Seq.empty,
      Seq(
        ("wm/concept/population_demographics/", THEME_SLOT),
        ("wm/process/development", PROCESS_SLOT)
      )
    )
  )

  tests.foreach { test => test.test() }
}
