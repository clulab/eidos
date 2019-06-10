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
      tester.test(group) should be(successful)
    }
    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    passingTest should "have correct moveFrom node" taggedAs (Somebody) in {
      tester.test(moveFrom) should be(successful)
    }
    passingTest should "have correct timeStart node" taggedAs (Somebody) in {
      tester.test(timeStart) should be(successful)
    }
    passingTest should "have correct timeEnd node" taggedAs (Somebody) in {
      tester.test(timeEnd) should be(successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful)
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
      tester.test(time1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }

    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be(successful)
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
      tester.test(group) should be(successful)
    }
    passingTest should "have correct moveFrom node" taggedAs (Somebody) in {
      tester.test(moveFrom) should be(successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful)
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
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct group1_3 node" taggedAs (Somebody) in {
      tester.test(group1_3) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }

    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be(successful)
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
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
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
      tester.test(group1_2) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct moveThrough2a node" taggedAs (Somebody) in {
      tester.test(moveThrough2a) should be(successful)
    }
    passingTest should "have correct moveThrough2b node" taggedAs (Somebody) in {
      tester.test(moveThrough2b) should be(successful)
    }
    passingTest should "have correct migration2a event" taggedAs (Somebody) in {
      tester.test(migration2a) should be(successful)
    }
    passingTest should "have correct migration2b event" taggedAs (Somebody) in {
      tester.test(migration2b) should be(successful)
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
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
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
      tester.test(moveTo) should be(successful)
    }
    passingTest should "have correct moveThrough node" taggedAs (Somebody) in {
      tester.test(moveThrough) should be(successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful)
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
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct moveTo2 node" taggedAs (Somebody) in {
      tester.test(moveTo2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
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
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  //
  // unit tests for Ethiopia 54660 fragments for hackathon
  //

  {
    val text = "* Between 1 and 11 March 2017, a total of 7,258 South Sudanese refugees have arrived in Gambella, Ethiopia, bringing the total who arrived since September 2016 to 68,858."

    val tester = new GraphTester(text)

    val time1 = NodeSpec("Between 1 and 11 March 2017")
    val group1 = NodeSpec("7,258 South Sudanese refugees")
    val moveTo1 = NodeSpec("Gambella, Ethiopia")
    val moveFrom1 = NodeSpec("South Sudanese")
    val migration1 = HumanMigrationEdgeSpec(
      time = Some(time1),
      group = Some(group1),
      moveTo = Some(moveTo1),
      moveFrom = Some(moveFrom1))

    val timeStart2 = NodeSpec("since September 2016")
    val group2 = NodeSpec("68,858")
    val migration2 = HumanMigrationEdgeSpec(
      time = Some(time2),
      group = Some(group2))

    behavior of "migration-az-hack-1a"

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be(successful)
    }
    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be(successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    behavior of "migration-az-hack-1b"

    passingTest should "have correct timeStart2 node" taggedAs (Somebody) in {
      tester.test(timeStart2) should be(successful)
    }
    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  {
    val text = "Of these, 3,967 arrived in the week of 6 to 11 March, representing a daily average arrival rate of 660 people."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("3,967")
    val time1 = NodeSpec("6 to 11 March")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      time = Some(time1))

    val group2 = NodeSpec("660 people")
    val groupMod2 = NodeSpec("daily")
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group2),
      groupMod2 = Some(groupMod2)
    )

    behavior of "migration-az-hack-2a"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be(successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    behavior of "migration-az-hack-2b"

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct groupMod2 node" taggedAs (Somebody) in {
      tester.test(groupMod2) should be(successful)
    }
    passingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  {
    val text = " All of them have been registered (level1) and most of them were relocated to Nguenyyiel refugee camp."

    val tester = new GraphTester(text)

    val moveTo1 = NodeSpec("Nguenyyiel refugee camp")
    val migration1 = HumanMigrationEdgeSpec(
      moveTo = Some(moveTo1))

    behavior of "migration-az-hack-3"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "192 level1 registered new arrivals remain in Pagak, awaiting relocation."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("192 level1 registered new arrivals")
    val moveTo1 = NodeSpec("Pagak") // should this be moveThrough ?
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1))

    behavior of "migration-az-hack-4"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = " The daily arrival rate has significantly jumped from 103 person in February and 199 in February to 660 so far in March."

    val tester = GraphTester(text)

    val group1 = NodeSpec("103 person")
    val groupMod1 = NodeSpec("daily")
    val time1 = NodeSpec("February")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      groupMod = Some(groupMod1),
      time = Some(time1))

    val group2 = NodeSpec("199")
    val groupMod2 = NodeSpec("daily")
    val time2 = NodeSpec("February")
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group1),
      groupMod = Some(groupMod2),
      time = Some(time2))

    val group3 = NodeSpec("660")
    val groupMod3 = NodeSpec("daily")
    val time3 = NodeSpec("March")
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(group3),
      groupMod = Some(groupMod3),
      time = Some(time3))

    behavior of "migration-az-hack-5a"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct groupMod1 node" taggedAs (Somebody) in {
      tester.test(groupMod1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    behavior of "migration-az-hack-5b"

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct groupMod2 node" taggedAs (Somebody) in {
      tester.test(groupMod2) should be (successful)
    }
    passingTest should "have correct time2 node" taggedAs (Somebody) in {
      tester.test(time2) should be (successful)
    }
    passingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    behavior of "migration-az-hack-5c"

    passingTest should "have correct group3 node" taggedAs (Somebody) in {
      tester.test(group3) should be (successful)
    }
    passingTest should "have correct groupMod3 node" taggedAs (Somebody) in {
      tester.test(groupMod3) should be (successful)
    }
    passingTest should "have correct time3 node" taggedAs (Somebody) in {
      tester.test(time3) should be (successful)
    }
    passingTest should "have correct migration3 node" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "* According to a recent sample survey conducted in Pagak, the new arrivals originated mainly from Upper Nile State (Nasir, Longechuk or Mathiang, Ulang and Maiwut Counties) and Jonglie State (Uror, Akobo and Ayod Counties)."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("the new arrivals")

    val moveTo1 = NodeSpec("Upper Nile State")
    val moveTo2 = NodeSpec("Nasir")
    val moveTo3 = NodeSpec("Longechuk")
    val moveTo4 = NodeSpec("Mathiang")
    val moveTo5 = NodeSpec("Ulang")
    val moveTo6 = NodeSpec("Maiwut Counties")

    val moveTo7 = NodeSpec("Jonglie State")
    val moveTo8 = NodeSpec("Uror")
    val moveTo9 = NodeSpec("Akobo")
    val moveTo10 = NodeSpec("Ayod Counties")

    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1)
    )
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo2)
    )
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo3)
    )
    val migration4 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo4)
    )
    val migration5 = HumanMigrationEdgeSpec(
      group = Some(group1)
        moveTo = Some(moveTo5)
    )
    val migration6 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo6)
    )
    val migration7 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo7)
    )
    val migration8 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo8)
    )
    val migration9 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo9)
    )
    val migration10 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo10)
    )

    behavior of "migration-az-hack-6"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct moveTo2 node" taggedAs (Somebody) in {
      tester.test(moveTo2) should be (successful)
    }
    passingTest should "have correct moveTo3 node" taggedAs (Somebody) in {
      tester.test(moveTo3) should be (successful)
    }
    passingTest should "have correct moveTo4 node" taggedAs (Somebody) in {
      tester.test(moveTo4) should be (successful)
    }
    passingTest should "have correct moveTo5 node" taggedAs (Somebody) in {
      tester.test(moveTo5) should be (successful)
    }
    passingTest should "have correct moveTo6 node" taggedAs (Somebody) in {
      tester.test(moveTo6) should be (successful)
    }
    passingTest should "have correct moveTo7 node" taggedAs (Somebody) in {
      tester.test(moveTo7) should be (successful)
    }
    passingTest should "have correct moveTo8 node" taggedAs (Somebody) in {
      tester.test(moveTo8) should be (successful)
    }
    passingTest should "have correct moveTo9 node" taggedAs (Somebody) in {
      tester.test(moveTo9) should be (successful)
    }
    passingTest should "have correct moveTo10 node" taggedAs (Somebody) in {
      tester.test(moveTo10) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
    passingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
    passingTest should "have correct migration3 node" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
    passingTest should "have correct migration4 node" taggedAs (Somebody) in {
      tester.test(migration4) should be (successful)
    }
    passingTest should "have correct migration5 node" taggedAs (Somebody) in {
      tester.test(migration5) should be (successful)
    }
    passingTest should "have correct migration6 node" taggedAs (Somebody) in {
      tester.test(migration6) should be (successful)
    }
    passingTest should "have correct migration7 node" taggedAs (Somebody) in {
      tester.test(migration7) should be (successful)
    }
    passingTest should "have correct migration8 node" taggedAs (Somebody) in {
      tester.test(migration8) should be (successful)
    }
    passingTest should "have correct migration9 node" taggedAs (Somebody) in {
      tester.test(migration9) should be (successful)
    }
    passingTest should "have correct migration10 node" taggedAs (Somebody) in {
      tester.test(migration10) should be (successful)
    }
  }

  {
    val text = "Conflict and food insecurity were cited as the main reasons for leaving South Sudan "

    val tester = new GraphTester(text)

    val moveFrom1 = NodeSpec("South Sudan")
    val migration1 = HumanMigrationEdgespec(
      moveFrom = Some(moveFrom1)
    )

    behavior of "migration-az-hack-7"

    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "* As of 15 March, Ethiopia hosted more than 356,000 South Sudanese refugees."

    val tester = new GraphTester(text)

    val timeEnd1 = NodeSpec("15 March")
    val moveTo1 = NodeSpec("Ethiopia")
    val group1 = NodeSpec("more than 356,000 South Sudanese refugees")
    val moveFrom1 = NodeSpec("South Sudanese")
    val migration1 = HumanMigrationEdgeSpec(
      timeEnd = Some(timeEnd1),
      moveTo = Some(moveTo1),
      group = Some(group1),
      moveFrom = Some(moveFrom1)
    )

    behavior of "migration-az-hack-8"

    passingTest should "have correct timeEnd1 node" taggedAs (Somebody) in {
      tester.test(timeEnd1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "* Latest developments: Between 1 February and 11 March 2017, a total of 12,828 refugees crossed through Pagak, 56.6% (7,258) of whom were registered in the first 11 days of March 2017."

    val tester = new GraphTester(text)

    val timeStart1 = NodeSpec("1 February")
    val timeEnd1 = NodeSpec("11 March 2017")
    val group1 = NodeSpec("12,828 refugees")
    val moveThrough1 = NodeSpec("Pagak")
    val migration1 = HumanMigrationEdgeSpec(
      timeStart = Some(timeStart1),
      timeEnd = Some(timeEnd1),
      group = Some(group1),
      moveThrough = Some(moveThrough1)
    )

    behavior of "migration-az-hack-8"

    passingTest should "have correct timeStart1 node" taggedAs (Somebody) in {
      tester.test(timeStart1) should be (successful)
    }
    passingTest should "have correct timeEnd1 node" taggedAs (Somebody) in {
      tester.test(timeEnd1) should be (successful)
    }
    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveThrough1 node" taggedAs (Somebody) in {
      tester.test(moveThrough1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = " Save for 192 people who are awaiting relocation, all the new arrivals were Level 1 registered and relocated to Nguenyyiel refugee camp."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("all the new arrivals")
    val moveTo1 = NodeSpec("Nguenyyiel refugee camp")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1)
    )

    behavior of "migration-az-hack-9"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = " This brings the number of South Sudanese refugees who have arrived in Ethiopia since September 2016 to 68,858. "

    val tester = new GraphTester(text)

    val group1 = NodeSpec("68,858")
    val moveFrom1 = NodeSpec("South Sudanese")
    val moveTo1 = NodeSpec("Ethiopia")
    val timeStart1 = NodeSpec("September 2016")
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      timeStart = Some(timeStart1)
    )

    behavior of "migration-az-hack-10"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    passingTest should "have correct timeStart1 node" taggedAs (Somebody) in {
      tester.test(timeStart1) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  // more sentences in doc need unit tests


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

}
