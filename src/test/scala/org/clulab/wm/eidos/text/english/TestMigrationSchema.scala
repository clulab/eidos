package org.clulab.wm.eidos.text.english

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestMigrationSchema extends EnglishTest {

  {
    val text = "Since the beginning of September 2016, almost 40,000 refugees arrived in Ethiopia from South Sudan as of mid-November."

    val tester = new GraphTester(text)

    val group = NodeSpec("almost 40,000 refugees")
    val moveTo = NodeSpec("Ethiopia")
    val moveFrom = NodeSpec("South Sudan")
    val timeStart = NodeSpec("the beginning of September 2016")
    val timeEnd = NodeSpec("mid-November")
    val migration = HumanMigrationEdgeSpec(group = Some(group),
        moveTo = Some(moveTo), moveFrom = Some(moveFrom),
        timeStart = Some(timeStart), timeEnd = Some(timeEnd))

    behavior of "migration-verbs1"

    passingTest should "have correct group node" taggedAs (Somebody) in {
      tester.test(group) should be (successful)
    }
    passingTest should "have correct moveTo node" taggedAs(Somebody) in {
      tester.test(moveTo) should be (successful)
    }
    passingTest should "have correct moveFrom node" taggedAs(Somebody) in {
      tester.test(moveFrom) should be (successful)
    }
    passingTest should "have correct timeStart node" taggedAs (Somebody) in {
      tester.test(timeStart) should be (successful)
    }
    passingTest should "have correct timeEnd node" taggedAs (Somebody) in {
      tester.test(timeEnd) should be (successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be (successful)
    }
  }

  {
    val text = "For many FGD respondents from Greater Equatoria, this was the second or even third time they or their family had been displaced out of South Sudan; many reported leaving during the 2013 displacement crisis and/or in the second Sudanese Civil War (1983-2006)."

    val tester = new GraphTester(text)

    val time1 = NodeSpec("2013")
    val migration1 = HumanMigrationEdgeSpec(time = Some(time1))

    val group2 = NodeSpec("family")
    val moveFrom2 = NodeSpec("South Sudan")
    // This one matches the rule.
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2))

    val migration3 = HumanMigrationEdgeSpec()

    behavior of "migration-verbs2"

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs(Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs(Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "IDPs from Uror also made their way to Duk, which hosted 24% of IDPs in March compared to 10% in February."

    val tester = new GraphTester(text)

    val group = NodeSpec("IDPs", NodeSpec.lastFilter) // There are two of these, so just pick the first one.
    val moveFrom = NodeSpec("Uror")
    val migration = HumanMigrationEdgeSpec(group = Some(group), moveFrom = Some(moveFrom))

    behavior of "migration_make-way"

    passingTest should "have correct group node" taggedAs (Somebody) in {
      tester.test(group) should be (successful)
    }
    passingTest should "have correct moveFrom node" taggedAs (Somebody) in {
      tester.test(moveFrom) should be (successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be (successful)
    }
  }

  {
    val text = "In March, Bor Town continued to receive IDPs displaced from the Equatorias, in particular Yei and populations returning from refugee settlements in Uganda."

    val tester = new GraphTester(text)

    val moveTo1 = NodeSpec("Bor Town")
    val group1_3 = NodeSpec("IDPs")
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(moveTo = Some(moveTo1), group = Some(group1_3))

    val group2 = NodeSpec("populations")
    val moveFrom2 = NodeSpec("Uganda")
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2))

    val migration3 = HumanMigrationEdgeSpec(group = Some(group1_3))

    behavior of "migration_receive"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct group1_3 node" taggedAs (Somebody) in {
      tester.test(group1_3) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs(Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs(Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "Based on regular visits by REACH to sites in Bor Town where IDPs have settled, as well as the continuous inflow of new arrivals, a conservative estimate would suggest that the number of IDPs from the Equatorias in Bor Town is likely to have been around 12,000-15,000 individuals at the end of March."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("IDPs", NodeSpec.lastFilter) // Last in the list of mentions, not necessarily last in text.
    val moveTo1 = NodeSpec("Bor Town", NodeSpec.firstFilter)
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1))

    val group2 = NodeSpec("new arrivals")
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2))

    behavior of "migration_settle"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs(Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
  }

  {
    val text = "Interviewed KIs passed through Nimule and Juba before settling in Bor."

    val tester = new GraphTester(text)

    val group1_2 = NodeSpec("Interviewed KIs")
    val moveTo1 = NodeSpec("Bor")
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1_2), moveTo = Some(moveTo1))

    // This is a hack.  Nothing checks that these are from the same mention.
    val moveThrough2a = NodeSpec("Nimule and")
    val moveThrough2b = NodeSpec("Juba")
    // This one(s) matches the rule.
    val migration2a = HumanMigrationEdgeSpec(group = Some(group1_2), moveThrough = Some(moveThrough2a))
    val migration2b = HumanMigrationEdgeSpec(group = Some(group1_2), moveThrough = Some(moveThrough2b))

    behavior of "migration_pass"

    passingTest should "have correct group1_2 node" taggedAs (Somebody) in {
      tester.test(group1_2) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs(Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct moveThrough2a node" taggedAs(Somebody) in {
      tester.test(moveThrough2a) should be (successful)
    }
    passingTest should "have correct moveThrough2b node" taggedAs(Somebody) in {
      tester.test(moveThrough2b) should be (successful)
    }
    passingTest should "have correct migration2a event" taggedAs (Somebody) in {
      tester.test(migration2a) should be (successful)
    }
    passingTest should "have correct migration2b event" taggedAs (Somebody) in {
      tester.test(migration2b) should be (successful)
    }
  }

  {
    val text = "* Departures: 375 individuals were recorded leaving Juba (52%) of which the vast majority intended to reach refugees camps in Uganda (75%)"

    val tester = new GraphTester(text)

    val group1 = NodeSpec("vast majority")
    val moveTo1 = NodeSpec("Uganda (75%")
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1))

    val group2 = NodeSpec("375 individuals")
    val moveFrom2 = NodeSpec("Juba")
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2))

    behavior of "migration_reach"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs(Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs(Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
  }

  {
    val text = "Additional cross border movement to Ethiopia occurs via Pagak."

    val tester = new GraphTester(text)

    val moveTo = NodeSpec("Ethiopia")
    val moveThrough = NodeSpec("Pagak")
    val migration = HumanMigrationEdgeSpec(moveTo = Some(moveTo), moveThrough = Some(moveThrough))

    behavior of "migration_nouns"

    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be (successful)
    }
    passingTest should "have correct moveThrough node" taggedAs (Somebody) in {
      tester.test(moveThrough) should be (successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be (successful)
    }
  }

  {
    val text = "* Arrivals: 358 individuals came from Uganda citing Juba as intended destination"

    val tester = new GraphTester(text)

    val group1 = NodeSpec("358 individuals")
    val moveFrom1 = NodeSpec("Uganda")
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveFrom = Some(moveFrom1))

    val moveTo2 = NodeSpec("Juba")
    // This one matches the rule.
    val migration2 = HumanMigrationEdgeSpec(moveTo = Some(moveTo2))

    behavior of "migration_destination-dep"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct moveTo2 node" taggedAs(Somebody) in {
      tester.test(moveTo2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
  }

  {
    val text = "* Arrivals: 36 individuals moved from Juba with the main destinations being Bor South in Jonglei State (24 individuals, or 67%) and Awerial (12 individuals, or 33%) in Lakes State"

    val tester = new GraphTester(text)

    val moveTo1 = NodeSpec("Bor South")
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(moveTo = Some(moveTo1))

    val group2 = NodeSpec("36 individuals")
    val moveFrom2 = NodeSpec("Juba")
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2))

    behavior of "migration_destination-right"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs(Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs(Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
  }

  //
  // new tests added by Andrew Z
  //

//  {
//    val text = "Since renewed fighting broke out across the country in July 2016, large numbers of refugees have poured into neighbouring countries, enlarging an already significant displacement crisis."
//
//    val tester = new GraphTester(text)
//
//    val group1 = NodeSpec("large numbers of refugees")
//    val moveTo1 = NodeSpec("neighbouring countries")
//    val timeStart1 = NodeSpec("July 2016")
//    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1), timeStart = Some(timeStart1))
//
//    val migration2 = HumanMigrationEdgeSpec()
//
//    behavior of "migration-supplemental-1"
//
//    passingTest should "have correct group1 node" taggedAs (Somebody) in {
//      tester.test(group1) should be (successful)
//    }
//    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
//      tester.test(moveTo1) should be (successful)
//    }
//    passingTest should "have correct timeStart1 node" taggedAs (Somebody) in {
//      tester.test(timeStart1) should be (successful)
//    }
//    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
//      tester.test(migration1) should be (successful)
//    }
//
//    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
//      tester.test(migration2) should be (successful)
//    }
//  }

//  {
//    val text = "By early-2017, nearly 60,000 people were fleeing the country each month, resulting in mass depopulation of both urban and rural areas."
//
//    val tester = new GraphTester(text)
//
//    val group1 = NodeSpec("nearly 60,000 people")
//    val moveFrom1 = NodeSpec("country")
//    val timeEnd1 = NodeSpec("early-2017")
//    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveFrom1), timeStart = Some(timeEnd1))
//
//    behavior of "migration-supplemental-2"
//
//    passingTest should "have correct group1 node" taggedAs (Somebody) in {
//      tester.test(group1) should be (successful)
//    }
//    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
//      tester.test(moveFrom1) should be (successful)
//    }
//    passingTest should "have correct timeEnd1 node" taggedAs (Somebody) in {
//      tester.test(timeEnd1) should be (successful)
//    }
//    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
//      tester.test(migration1) should be (successful)
//    }
//  }

//  {
//    val text = "Civilians caught in the fighting between armed actors first displaced into remote areas of the bush, then flee to Uganda, Kenya or the DRC over the course of a few months."
//
//    val tester = new GraphTester(text)
//
//    val group1 = NodeSpec("Civilians")
//    val moveTo1 = NodeSpec("remote areas of the bush")
//    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1))
//
//    val moveTo2 = NodeSpec("Uganda")
//    val migration2 = HumanMigrationEdgeSpec(moveTo = Some(moveTo2))
//
//    val moveTo3 = NodeSpec("Kenya")
//    val migration3 = HumanMigrationEdgeSpec(moveTo = Some(moveTo3))
//
//    val moveTo4 = NodeSpec("DRC")
//    val migration4 = HumanMigrationEdgeSpec(moveTo = Some(moveTo4))
//
//
//    behavior of "migration-supplemental-3"
//
//    passingTest should "have correct group1 node" taggedAs (Somebody) in {
//      tester.test(group1) should be (successful)
//    }
//    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
//      tester.test(moveTo1) should be (successful)
//    }
//    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
//      tester.test(migration1) should be (successful)
//    }
//
//    passingTest should "have correct moveTo2" taggedAs (Somebody) in {
//      tester.test(moveTo2) should be (successful)
//    }
//    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
//      tester.test(migration2) should be (successful)
//    }
//
//    passingTest should "have correct moveTo3" taggedAs (Somebody) in {
//      tester.test(moveTo3) should be (successful)
//    }
//    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
//      tester.test(migration3) should be (successful)
//    }
//
//    passingTest should "have correct moveTo4" taggedAs (Somebody) in {
//      tester.test(moveTo4) should be (successful)
//    }
//    passingTest should "have correct migration4 event" taggedAs (Somebody) in {
//      tester.test(migration4) should be (successful)
//    }
//  }

  //
  // new tests by Mihai
  //

  {
    val text = "*  Between 1 and 30 March 2017, 16,274 South Sudanese refugees arrived in Gambella, Ethiopia, bringing the total number of new arrivals since September 2016 to 77,874."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("16,274 South Sudanese refugees")
    val time1 = NodeSpec("Between 1 and 30 March 2017")
    val moveFrom1 = NodeSpec("South Sudanese")
    val moveTo1 = NodeSpec("Gambella")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      time = Some(time1))

    behavior of "migration-arrive"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = " In the past week, the daily arrival average stood at 508 individuals."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("508 individuals")
    val groupMod1 = NodeSpec("daily")
    val time1 = NodeSpec("the past week")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      groupModifier = Some(groupMod1),
      time = Some(time1))

    behavior of "migration-arrival"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct groupModifier1 node" taggedAs (Somebody) in {
      tester.test(groupMod1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "Except for 246 individuals who are awaiting relocation and 200 others awaiting level 1 registration in Pagak, all new arrivals have been relocated to Nguenyyiel Refugee Camp."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("new arrivals")
    val moveTo1 = NodeSpec("Nguenyyiel Refugee Camp")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1)
    )

    behavior of "migration-relocate"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "• As of 30 March 2017, Ethiopia hosted around 365,600 South Sudanese refugees."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("365,600 South Sudanese refugees")
    val moveTo1 = NodeSpec("Ethiopia")
    val moveFrom1 = NodeSpec("South Sudanese")
    val time1 = NodeSpec("30 March 2017")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1),
      moveFrom = Some(moveFrom1),
      time = Some(time1)
    )

    behavior of "migration-hosted"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "They originate mostly from the Upper Nile, Jonglei and Unity states."

    val tester = new GraphTester(text)

    val moveFrom1 = NodeSpec("Upper Nile")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(moveFrom1),
    )

    behavior of "migration-originate"

    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    val moveFrom2 = NodeSpec("Jonglei")
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(moveFrom2),
    )

    behavior of "migration-originate"

    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    val moveFrom3 = NodeSpec("Upper Nile")
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(moveFrom3),
    )

    behavior of "migration-originate"

    passingTest should "have correct moveFrome3 node" taggedAs (Somebody) in {
      tester.test(moveFrom3) should be (successful)
    }
    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "• During the past week, 4,608 new arrivals were relocated from Pagak to Nguenyyiel camp, with 246 individuals awaiting relocation as of 30 March 2017."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("4,608 new arrival")
    val moveFrom1 = NodeSpec("Pagak")
    val moveTo1 = NodeSpec("Nguenyyiel camp")
    val time1 = NodeSpec("past week")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      time = Some(time1)
    )

    behavior of "migration-relocate"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  /*
  // template for new tests
  {
    val text = ""

    val tester = new GraphTester(text)

    val group1 = NodeSpec("")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
    )

    behavior of "migration-TODO"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  */


}
