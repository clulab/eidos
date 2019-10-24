package org.clulab.wm.eidos.text.english

import org.clulab.wm.eidos.attachments.{CountModifier, CountUnit}
import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestMigrationSchema extends EnglishTest {

  {
    val text = "Since the beginning of September 2016, almost 40,000 refugees arrived in Ethiopia from South Sudan as of mid-November."

    val tester = new GraphTester(text)

    val group = NodeSpec("almost 40,000 refugees", CountSpec(40000, CountModifier.Max))
    val moveTo = NodeSpec("Ethiopia", GeoLoc("Ethiopia"))
    val moveFrom = NodeSpec("South Sudan", GeoLoc("South Sudan"))
    val timeStart = NodeSpec("September 2016", TimEx("September 2016"))
    val timeEnd = NodeSpec("mid-November", TimEx("mid-November"))
    val migration = HumanMigrationEdgeSpec(group = Some(group),
      moveTo = Some(moveTo), moveFrom = Some(moveFrom),
      timeStart = Some(timeStart), timeEnd = Some(timeEnd)) //fixme: time is found as "beginning of sept.."

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
    failingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful) //time is found as "the beginning of September 2016" and the timex's are not attached
    }
  }

  {
    val text = "For many FGD respondents from Greater Equatoria, this was the second or even third time they or their family had been displaced out of South Sudan; many reported leaving during the 2013 displacement crisis and/or in the second Sudanese Civil War (1983-2006)."

    val tester = new GraphTester(text)

    val time1 = NodeSpec("2013", TimEx("2013"))
    val time2 = NodeSpec("1983-2006") //what would timex be?
    val group = NodeSpec("FGD respondents") //fixme: is in a different clause, need to attach somehow
    val moveFrom = NodeSpec("South Sudan", GeoLoc("South Sudan"))
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(group = Some(group), moveFrom = Some(moveFrom), time = Some(time1))

    val migration2 = HumanMigrationEdgeSpec(group = Some(group), moveFrom = Some(moveFrom), time = Some(time2))

    behavior of "migration-verbs2"

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be(successful)
    }
    failingTest should "have correct group node" taggedAs (Somebody) in {
      tester.test(group) should be(successful)
    }
    passingTest should "have correct moveFrom node" taggedAs (Somebody) in {
      tester.test(moveFrom) should be(successful)
    }
    failingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  {
    val text = "IDPs from Uror also made their way to Duk."
    val tester = new GraphTester(text)

    val group = NodeSpec("IDPs from Uror", NodeSpec.lastFilter) //TODO: is it ok to include "from Uror" here ?
    val moveFrom = NodeSpec("Uror", GeoLoc("Uror")) //fixme: uror is found with location-backoff => didn't find geoattachment
    val moveTo = NodeSpec("Duk", GeoLoc("Duk"))
    val migration = HumanMigrationEdgeSpec(group = Some(group), moveFrom = Some(moveFrom), moveTo = Some(moveTo))

    behavior of "migration_make-way"

    failingTest should "have correct group node" taggedAs (Somebody) in {
      tester.test(group) should be(successful)
    }
    passingTest should "have correct moveFrom node" taggedAs (Somebody) in {
      tester.test(moveFrom) should be(successful)
    }
    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    failingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful)
    }
  }

  {
    val text = "In March, Bor Town continued to receive IDPs displaced from the Equatorias, in particular Yei and populations returning from refugee settlements in Uganda."
    //TODO: the parse is really bad for this sentence, so it might be hard to make this test pass
    //fixme: merging interference
    val tester = new GraphTester(text)

    val moveTo = NodeSpec("Bor Town", GeoLoc("Bor Town"))
    val group1 = NodeSpec("IDPs")
    val time = NodeSpec("In March", TimEx("In March"))
    val moveFrom1 = NodeSpec("Equatorias", GeoLoc("Equatorias"))
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(moveTo = Some(moveTo), group = Some(group1), time = Some(time), moveFrom = Some(moveFrom1))

    val group2 = NodeSpec("populations")
    val moveFrom2 = NodeSpec("Uganda", GeoLoc("Uganda"))
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2), moveTo = Some(moveTo), time = Some(time))


    behavior of "migration_receive"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }

  }

  {
    val text = "Based on regular visits by REACH to sites in Bor Town where IDPs have settled, as well as the continuous inflow of new arrivals, a conservative estimate would suggest that the number of IDPs from the Equatorias in Bor Town is likely to have been around 12,000-15,000 individuals at the end of March."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("IDPs", NodeSpec.lastFilter) // Last in the list of mentions, not necessarily last in text.
  val moveTo = NodeSpec("Bor Town", GeoLoc("Bor Town"), NodeSpec.firstFilter)
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo))

    val group2 = NodeSpec("12,000-15,000 individuals") //todo: need the rule for the word number (but it's also in stop words)
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveTo = Some(moveTo))

    behavior of "migration_settle"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  {
    val text = "Interviewed KIs passed through Nimule and Juba before settling in Bor."

    val tester = new GraphTester(text)

    val group1_2 = NodeSpec("Interviewed KIs")
    val moveTo1 = NodeSpec("Bor", GeoLoc("Bor"))
//    val migration1 = HumanMigrationEdgeSpec(group = Some(group1_2), moveTo = Some(moveTo1)) -> this is wrong

    // This is a hack.  Nothing checks that these are from the same mention.
    val moveThrough2a = NodeSpec("Nimule", GeoLoc("Nimule"))
    val moveThrough2b = NodeSpec("Juba", GeoLoc("Juba"))
    // This one(s) matches the rule.
    val migration2a = HumanMigrationEdgeSpec(group = Some(group1_2), moveThrough = Some(moveThrough2a), moveTo = Some(moveTo1))
    val migration2b = HumanMigrationEdgeSpec(group = Some(group1_2), moveThrough = Some(moveThrough2b), moveTo = Some(moveTo1))

    behavior of "migration_pass"

    passingTest should "have correct group1_2 node" taggedAs (Somebody) in {
      tester.test(group1_2) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
//    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
//      tester.test(migration1) should be(successful)
//    }

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
    val text = "Departures: 375 individuals were recorded leaving Juba (52%) of which the vast majority intended to reach refugees camps in Uganda (75%)"
    //fixme: not clear how many events should be extracted

    val tester = new GraphTester(text)

//    val group1 = NodeSpec("vast majority") //has been merged
    val moveTo = NodeSpec("Uganda", GeoLoc("Uganda"))
    // This one matches the rule.
//    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1))

    val group = NodeSpec("375 individuals", CountSpec(375))
    val moveFrom = NodeSpec("Juba", GeoLoc("Juba"))
    val migration = HumanMigrationEdgeSpec(group = Some(group), moveFrom = Some(moveFrom), moveTo = Some(moveTo))

    behavior of "migration_reach"

    passingTest should "have correct group node" taggedAs (Somebody) in {
      tester.test(group) should be(successful)
    }
    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }

    passingTest should "have correct moveFrom event" taggedAs (Somebody) in {
      tester.test(moveFrom) should be(successful)
    }
    passingTest should "have correct migration event" taggedAs (Somebody) in {
      tester.test(migration) should be(successful)
    }

//    passingTest should "have correct group2 node" taggedAs (Somebody) in {
//      tester.test(group2) should be(successful)
//    }
//    failingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
//      tester.test(moveFrom2) should be(successful)
//    }
//    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
//      tester.test(migration2) should be(successful)
//    }
  }

  {
    val text = "Additional cross border movement to Ethiopia occurs via Pagak."

    val tester = new GraphTester(text)

    val moveTo = NodeSpec("Ethiopia", GeoLoc("Ethiopia"))
    val moveThrough = NodeSpec("Pagak", GeoLoc("Pagak")) //fixme: didn't find geoattachment
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
    val text = "Arrivals: 358 individuals came from Uganda citing Juba as intended destination"

    val tester = new GraphTester(text)

    val group1 = NodeSpec("358 individuals", CountSpec(358))
    val moveFrom1 = NodeSpec("Uganda", GeoLoc("Uganda"))
    val moveTo = NodeSpec("Juba", GeoLoc("Juba"))
    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveFrom = Some(moveFrom1), moveTo = Some(moveTo))

    behavior of "migration_destination-dep"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be(successful)
    }

    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

  }

  {
    val text = "Arrivals: 36 individuals moved from Juba with the main destinations being Bor South in Jonglei State (24 individuals, or 67%) and Awerial (12 individuals, or 33%) in Lakes State"
    //fixme: need to decide on the ideal extraction
    //todo: update the test
    val tester = new GraphTester(text)

    val moveTo1 = NodeSpec("Bor South", GeoLoc("Bor South")) //fixme: Bor South not found
    // This one matches the rule.
    val migration1 = HumanMigrationEdgeSpec(moveTo = Some(moveTo1))

    val group2 = NodeSpec("36 individuals", CountSpec(36))
    val moveFrom2 = NodeSpec("Juba", GeoLoc("Juba"))
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveFrom = Some(moveFrom2)) //fixme: should this include any moveTos?

    behavior of "migration_destination-right"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be(successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be(successful)
    }
    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful)
    }
  }

  //
  // unit tests for Ethiopia 54660 fragments for hackathon
  //

  {
    val text = "Between 1 and 11 March 2017, a total of 7,258 South Sudanese refugees have arrived in Gambella, Ethiopia, bringing the total who arrived since September 2016 to 68,858."

    val tester = new GraphTester(text)

    val time1 = NodeSpec("Between 1 and 11 March 2017", TimEx("Between 1 and 11 March 2017"))
    val group1 = NodeSpec("total of 7,258 South Sudanese refugees", CountSpec(7258))
    val moveTo = NodeSpec("Gambella", GeoLoc("Gambella")) //todo: add ", Ethiopia"?
    val moveFrom1 = NodeSpec("South Sudanese", GeoLoc("South Sudan"))
    val migration1 = HumanMigrationEdgeSpec(
      time = Some(time1),
      group = Some(group1),
      moveTo = Some(moveTo),
      moveFrom = Some(moveFrom1))

    val timeStart2 = NodeSpec("since September 2016", TimEx("since September 2016"))
    val group2 = NodeSpec("68,858", CountSpec(68858))
    val migration2 = HumanMigrationEdgeSpec(
      timeStart = Some(timeStart2),
      group = Some(group2), moveTo = Some(moveTo))

    behavior of "migration-az-hack-1a"

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be(successful)
    }
    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo) should be(successful)
    }
    failingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be(successful)
    }
    failingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful) //fails because timeStart2 attaches to this first event---these are two different time arguments, so the bothSpecific anti-fragment-merge heuristic does not work todo: fix time
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
    //todo: need a rule for rates of arrivals

    val tester = new GraphTester(text)

    val group1 = NodeSpec("3,967", CountSpec(3967))
    val time1 = NodeSpec("week of 6 to 11 March", TimEx("week of 6 to 11 March"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      time = Some(time1))

    val group2 = NodeSpec("660 people", CountSpec(660, CountModifier.NoModifier, CountUnit.Daily))
    val groupMod2 = NodeSpec("daily", TimEx("daily")) //todo: decide what to do with groupMods---the info goes into the group attachment and will be lost if we don't find the group modifier this way
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group2),
      groupModifier = Some(groupMod2)
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
//    passingTest should "have correct groupMod2 node" taggedAs (Somebody) in {
//      tester.test(groupMod2) should be(successful)
//    }
    passingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be(successful) //fixme: fails for unknown reason
    }
  }

  {
    val text = "All of them have been registered (level1) and most of them were relocated to Nguenyyiel refugee camp."

    val tester = new GraphTester(text)

    val moveTo1 = NodeSpec("Nguenyyiel", GeoLoc("Nguenyyiel")) //fixme: doesn't have attachments
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

    val group1 = NodeSpec("192 level1 registered new arrivals", CountSpec(192)) //fixme: didn't expand to full NP
    val moveTo1 = NodeSpec("Pagak", GeoLoc("Pagak")) // should this be moveThrough ? fixme: didn't find the attachment
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1))

    behavior of "migration-az-hack-4"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    failingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "The daily arrival rate has significantly jumped from 103 person in February and 199 in February to 660 so far in March."
    //todo: hard to parse---try to update the arrival rule or the rate rule
    //fixme: group expands too much
    //todo: rewrite the test

    val tester = new GraphTester(text)

    val group1 = NodeSpec("103 person", CountSpec(103, CountModifier.NoModifier, CountUnit.Daily))
//    val groupMod1 = NodeSpec("daily")
    val time1 = NodeSpec("February", TimEx("February"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
//      groupModifier = Some(groupMod1),
      time = Some(time1))

    val group2 = NodeSpec("199", CountSpec(199, CountModifier.NoModifier, CountUnit.Daily))
//    val groupMod2 = NodeSpec("daily")
    val time2 = NodeSpec("February", TimEx("February"))
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group1),
//      groupModifier = Some(groupMod2),
      time = Some(time2))

    val group3 = NodeSpec("660", CountSpec(660, CountModifier.NoModifier, CountUnit.Daily))
//    val groupMod3 = NodeSpec("daily")
    val time3 = NodeSpec("March", TimEx("March"))
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(group3),
//      groupModifier = Some(groupMod3),
      time = Some(time3))

    behavior of "migration-az-hack-5a"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
//    passingTest should "have correct groupMod1 node" taggedAs (Somebody) in {
//      tester.test(groupMod1) should be (successful)
//    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    failingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    behavior of "migration-az-hack-5b"

    failingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }
//    passingTest should "have correct groupMod2 node" taggedAs (Somebody) in {
//      tester.test(groupMod2) should be (successful)
//    }
    passingTest should "have correct time2 node" taggedAs (Somebody) in {
      tester.test(time2) should be (successful)
    }
    failingTest should "have correct migration2 node" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    behavior of "migration-az-hack-5c"

    failingTest should "have correct group3 node" taggedAs (Somebody) in {
      tester.test(group3) should be (successful)
    }
//    passingTest should "have correct groupMod3 node" taggedAs (Somebody) in {
//      tester.test(groupMod3) should be (successful)
//    }
    passingTest should "have correct time3 node" taggedAs (Somebody) in {
      tester.test(time3) should be (successful)
    }
    failingTest should "have correct migration3 node" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "According to a recent sample survey conducted in Pagak, the new arrivals originated mainly from Upper Nile State (Nasir, Longechuk or Mathiang, Ulang and Maiwut Counties) and Jonglei State (Uror, Akobo and Ayod Counties)."
    //todo: rule for `according to'
    val tester = new GraphTester(text)
    val moveTo = NodeSpec("Pagak", GeoLoc("Pagak"))
    val group1 = NodeSpec("new arrivals") //leave of "the" ?

    val moveFrom1 = NodeSpec("Upper Nile State", GeoLoc("Upper Nile State"))
    val moveFrom2 = NodeSpec("Nasir", GeoLoc("Nasir")) //fixme not found as location, didn't find an attachment
    val moveFrom3 = NodeSpec("Longechuk", GeoLoc("Longechuk"))
    val moveFrom4 = NodeSpec("Mathiang", GeoLoc("Mathiang"))
    val moveFrom5 = NodeSpec("Ulang", GeoLoc("Ulang"))
    val moveFrom6 = NodeSpec("Maiwut Counties", GeoLoc("Maiwut Counties"))

    val moveFrom7 = NodeSpec("Jonglei State", GeoLoc("Jonglei State")) //fixme: only State is found as geoloc
    val moveFrom8 = NodeSpec("Uror", GeoLoc("Uror"))
    val moveFrom9 = NodeSpec("Akobo", GeoLoc("Akobo"))
    val moveFrom10 = NodeSpec("Ayod Counties", GeoLoc("Ayod Counties"))

    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo)
    )
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom2),
      moveTo = Some(moveTo)
    )
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom3),
      moveTo = Some(moveTo)
    )
    val migration4 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom4),
      moveTo = Some(moveTo)
    )
    val migration5 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom5),
      moveTo = Some(moveTo)
    )
    val migration6 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom6),
      moveTo = Some(moveTo)
    )
    val migration7 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom7),
      moveTo = Some(moveTo)
    )
    val migration8 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom8),
      moveTo = Some(moveTo)
    )
    val migration9 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom9),
      moveTo = Some(moveTo)
    )
    val migration10 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom10),
      moveTo = Some(moveTo)
    )

    behavior of "migration-az-hack-6"

    passingTest should "have correct moveTo node" taggedAs (Somebody) in {
      tester.test(moveTo) should be (successful)
    }

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    passingTest should "have correct moveFrom3 node" taggedAs (Somebody) in {
      tester.test(moveFrom3) should be (successful)
    }
    passingTest should "have correct moveFrom4 node" taggedAs (Somebody) in {
      tester.test(moveFrom4) should be (successful)
    }
    passingTest should "have correct moveFrom5 node" taggedAs (Somebody) in {
      tester.test(moveFrom5) should be (successful)
    }
    passingTest should "have correct moveFrom6 node" taggedAs (Somebody) in {
      tester.test(moveFrom6) should be (successful)
    }
    passingTest should "have correct moveFrom7 node" taggedAs (Somebody) in {
      tester.test(moveFrom7) should be (successful)
    }
    passingTest should "have correct moveFrom8 node" taggedAs (Somebody) in {
      tester.test(moveFrom8) should be (successful)
    }
    passingTest should "have correct moveFrom9 node" taggedAs (Somebody) in {
      tester.test(moveFrom9) should be (successful)
    }
    passingTest should "have correct moveFrom10 node" taggedAs (Somebody) in {
      tester.test(moveFrom10) should be (successful)
    }
    passingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
    failingTest should "have correct migration2 node" taggedAs (Somebody) in {
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
    failingTest should "have correct migration7 node" taggedAs (Somebody) in {
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

    val moveFrom1 = NodeSpec("South Sudan", GeoLoc("South Sudan"))
    val migration1 = HumanMigrationEdgeSpec(
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
    val text = "As of 15 March, Ethiopia hosted more than 356,000 South Sudanese refugees."

    val tester = new GraphTester(text)

    val timeEnd1 = NodeSpec("15 March", TimEx("15 March"))
    val moveTo1 = NodeSpec("Ethiopia", GeoLoc("Ethiopia"))
    val group1 = NodeSpec("more than 356,000 South Sudanese refugees", CountSpec(356000, CountModifier.Min, CountUnit.Absolute)) //fixme: fails for unknown reason
    val moveFrom1 = NodeSpec("South Sudanese", GeoLoc("South Sudanese")) //fixme: can't get geoloc from adjectives
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
    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    failingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "Latest developments: Between 1 February and 11 March 2017, a total of 12,828 refugees crossed through Pagak, 56.6% (7,258) of whom were registered in the first 11 days of March 2017."

    //In this test, the time is not getting attached to the first migration event because of a bad parse (the clause starting with 'a total of...' is found as appos, so there is no good path from the migration to the time)
    //todo: need a rule for 'register'; difficult parse; need to decide on ideal extraction
    val tester = new GraphTester(text)

    val time1 = NodeSpec("Between 1 February and 11 March 2017", TimEx("Between 1 February and 11 March 2017"))
    val group1 = NodeSpec("12,828 refugees", CountSpec(12828))
    val moveThrough1 = NodeSpec("Pagak", GeoLoc("Pagak")) //fixme: didn't find the attachment
    val migration1 = HumanMigrationEdgeSpec(
      time = Some(time1),
      group = Some(group1),
      moveThrough = Some(moveThrough1)
    )

    behavior of "migration-az-hack-9"

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
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
    val text = "Save for 192 people who are awaiting relocation, all the new arrivals were Level 1 registered and relocated to Nguenyyiel refugee camp."
    //fixme: negative group rule?
    //todo: bad parse
    //todo: update the test

    val tester = new GraphTester(text)

    val group1 = NodeSpec("all the new arrivals")
    val moveTo1 = NodeSpec("Nguenyyiel", GeoLoc("Nguenyyiel"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1)
    )

    behavior of "migration-az-hack-10"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    failingTest should "have correct migration1 node" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "This brings the number of refugees who have arrived in Ethiopia since September 2016 to 68,858."
    //fixme: if there's a conflicting specific group ("South Sudanese refugees"), there are two migration events prevented from being merged by the 'bothSpecific' heuristic

    val tester = new GraphTester(text)

    val group1 = NodeSpec("68,858", CountSpec(68858))
//    val moveFrom1 = NodeSpec("South Sudanese", GeoLoc("South Sudanese"))
    val moveTo1 = NodeSpec("Ethiopia", GeoLoc("Ethiopia"))
    val timeStart1 = NodeSpec("since September 2016", TimEx("since September 2016"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
//      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      timeStart = Some(timeStart1)
    )

    behavior of "migration-az-hack-11"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
//    failingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
//      tester.test(moveFrom1) should be (successful)
//    }
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

  //
  // new tests by Mihai
  //

  {
    val text = "Between 1 and 30 March 2017, 16,274 South Sudanese refugees arrived in Gambella, Ethiopia, bringing the total number of new arrivals since September 2016 to 77,874."
    //todo: test for the second clause?
    val tester = new GraphTester(text)

    val group1 = NodeSpec("16,274 South Sudanese refugees", CountSpec(16274)) //fixme: location attachment found but probably shouldn't be
    val time1 = NodeSpec("Between 1 and 30 March 2017", TimEx("Between 1 and 30 March 2017"))
    val moveFrom1 = NodeSpec("South Sudanese", GeoLoc("South Sudanese")) //fixme: doesn't find locations expressed through an adjective
    val moveTo1 = NodeSpec("Gambella", GeoLoc("Gambella"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      time = Some(time1))


    val group2 = NodeSpec("77,874", CountSpec(77874))
    val timeStart = NodeSpec("since September 2016", TimEx("since September 2016"))
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group2),
      moveTo = Some(moveTo1),
      timeStart = Some(timeStart))

    behavior of "migration-arrive"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    failingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }

    passingTest should "have correct timeStart node" taggedAs (Somebody) in {
      tester.test(timeStart) should be (successful)
    }
    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }
  }

  {
    val text = "In the past week, the daily arrival average stood at 508 individuals."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("508 individuals", CountSpec(508, CountModifier.NoModifier, CountUnit.Daily))
    val groupMod1 = NodeSpec("daily", TimEx("daily"))
    val time1 = NodeSpec("past week", TimEx("past week"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      groupModifier = Some(groupMod1),
      time = Some(time1))

    behavior of "migration-arrival"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
//    passingTest should "have correct groupModifier1 node" taggedAs (Somebody) in {
//      tester.test(groupMod1) should be (successful)
//    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "Except for 246 individuals who are awaiting relocation and 200 others awaiting level 1 registration in Pagak, all new arrivals have been relocated to Nguenyyiel Refugee Camp."
    //todo: negative number rule?
    //todo: found two events with different moveTo's: Nguenyyiel AND Refugee Camp
    val tester = new GraphTester(text)

    val group1 = NodeSpec("new arrivals")
    val moveTo1 = NodeSpec("Nguenyyiel", GeoLoc("Nguenyyiel")) //fixme: should fail but passes; should be Nguenyyiel Refugee Camp?
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
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "As of 30 March 2017, Ethiopia hosted around 365,600 South Sudanese refugees."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("365,600 South Sudanese refugees", CountSpec(365600, CountModifier.Approximate))
    val moveTo1 = NodeSpec("Ethiopia", GeoLoc("Ethiopia"))
    val moveFrom1 = NodeSpec("South Sudanese") //fixme: can't extract location from an adjective; now "South Sudanese" is found as GeoLoc bc of the gazetteer
    val time1 = NodeSpec("30 March 2017", TimEx("30 March 2017"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveTo = Some(moveTo1),
      moveFrom = Some(moveFrom1),
      time = Some(time1)
    )

    behavior of "migration-hosted"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }
    failingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "They originate mostly from the Upper Nile, Jonglei and Unity states."
    val tester = new GraphTester(text)

    val moveFrom1 = NodeSpec("Upper Nile", GeoLoc("Upper Nile")) //fixme: didn't find the attachment
    val migration1 = HumanMigrationEdgeSpec(
      moveFrom = Some(moveFrom1)
    )

    behavior of "migration-originate1"

    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    val moveFrom2 = NodeSpec("Jonglei", GeoLoc("Jonglei")) //fixme: didn't find the attachment
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(moveFrom2)
    )

    behavior of "migration-originate2"

    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

    val moveFrom3 = NodeSpec("Upper Nile", GeoLoc("Upper Nile")) //fixme: didn't find the attachment
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(moveFrom3)
    )

    behavior of "migration-originate3"

    passingTest should "have correct moveFrom3 node" taggedAs (Somebody) in {
      tester.test(moveFrom3) should be (successful)
    }
    failingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }
  }

  {
    val text = "During the past week, 4,608 new arrivals were relocated from Pagak to Nguenyyiel camp, with 246 individuals awaiting relocation as of 30 March 2017."
    //fixme: possible merge interference

    val tester = new GraphTester(text)

    val group1 = NodeSpec("4,608 new arrivals", CountSpec(4608))
    val moveFrom1 = NodeSpec("Pagak", GeoLoc("Pagak"))
    val moveTo1 = NodeSpec("Nguenyyiel", GeoLoc("Nguenyyiel"))
    val time1 = NodeSpec("past week", TimEx("past week"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      time = Some(time1)
    )

    behavior of "migration-relocate2"

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

  {
    val text = "* Between 13 and 28 April 2017, 3,604 South Sudanese refugees arrived in Gambella, Ethiopia."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("3,604 South Sudanese refugees", CountSpec(3604))
    val time1 = NodeSpec("Between 13 and 28 April 2017", TimEx("Between 13 and 28 April 2017"))
    val moveFrom1 = NodeSpec("South Sudanese", GeoLoc("South Sudanese")) //fixme: can't extract locations from adjectives
    val moveTo1 = NodeSpec("Gambella", GeoLoc("Gambella"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1),
      moveTo = Some(moveTo1),
      time = Some(time1))

    behavior of "migration-arrive2"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
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
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "Currently, the average daily rate of arrivals is 350 individuals."
    //fixme: potential merge interference (possibly rate + arrival)
    val tester = new GraphTester(text)

    val group1 = NodeSpec("350 individuals", CountSpec(350, CountModifier.NoModifier, CountUnit.Daily)) //fixme: expands to `arrivals is 350 individuals"
//    val groupMod1 = NodeSpec("daily")
    val time1 = NodeSpec("Currently", TimEx("Currently")) // this may be overkill?
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
//      groupModifier = Some(groupMod1),
      time = Some(time1))

    behavior of "migration-arrival2"

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
//    passingTest should "have correct groupModifier1 node" taggedAs (Somebody) in {
//      tester.test(groupMod1) should be (successful)
//    }
    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "The remaining 1% were registered to have fled from Unity State."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("remaining 1%", CountSpec(1, CountModifier.NoModifier, CountUnit.Percentage))
    val moveFrom1 = NodeSpec("Unity State", GeoLoc("Unity State")) //fixme: didn't find attachment
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1))

    behavior of "migration-fled"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "85% of the new arrivals originated from Upper Nile State (Nasir, Longechuk or Mathiang, Ulang and Maiwut Counties), whilst 14% came from Jonglei State (Uror, Akobo and Ayod Counties)."
    //fixme: refine the originate rule to include multiple locations?
    //fixme: need to decide on ideal extraction (e.g., Nasir is part of Upper Nile State => one event or two events?

    val tester = new GraphTester(text)

    val group1 = NodeSpec("85% of the new arrivals", CountSpec(85, CountModifier.NoModifier, CountUnit.Percentage))
    val moveFrom1 = NodeSpec("Upper Nile State", GeoLoc("Upper Nile State"))
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom1))

    behavior of "migration-originated"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }
    passingTest should "have correct moveFrom1 node" taggedAs (Somebody) in {
      tester.test(moveFrom1) should be (successful)
    }
    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    val moveFrom2 = NodeSpec("Nasir", GeoLoc("Nasir"))
    val migration2 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom2))

    behavior of "migration-originated2"


    passingTest should "have correct moveFrom2 node" taggedAs (Somebody) in {
      tester.test(moveFrom2) should be (successful)
    }
    failingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }


    val moveFrom3 = NodeSpec("Longechuk", GeoLoc("Longechuk"))
    val migration3 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom3))

    behavior of "migration-originated3"

    passingTest should "have correct moveFrom3 node" taggedAs (Somebody) in {
      tester.test(moveFrom3) should be (successful)
    }
    passingTest should "have correct migration3 event" taggedAs (Somebody) in {
      tester.test(migration3) should be (successful)
    }

    val moveFrom4 = NodeSpec("Mathiang", GeoLoc("Mathiang"))
    val migration4 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom4))

    behavior of "migration-originated4"

    passingTest should "have correct moveFrom4 node" taggedAs (Somebody) in {
      tester.test(moveFrom4) should be (successful)
    }
    passingTest should "have correct migration4 event" taggedAs (Somebody) in {
      tester.test(migration4) should be (successful)
    }

    val moveFrom5 = NodeSpec("Ulang", GeoLoc("Ulang"))
    val migration5 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom5))

    behavior of "migration-originated5"

    passingTest should "have correct moveFrom5 node" taggedAs (Somebody) in {
      tester.test(moveFrom5) should be (successful)
    }
    passingTest should "have correct migration5 event" taggedAs (Somebody) in {
      tester.test(migration5) should be (successful)
    }

    val moveFrom6 = NodeSpec("Maiwut Counties", GeoLoc("Maiwut Counties"))
    val migration6 = HumanMigrationEdgeSpec(
      group = Some(group1),
      moveFrom = Some(moveFrom6))

    behavior of "migration-originated6"

    failingTest should "have correct moveFrom6 node" taggedAs (Somebody) in {
      tester.test(moveFrom6) should be (successful)
    }
    failingTest should "have correct migration6 event" taggedAs (Somebody) in {
      tester.test(migration6) should be (successful)
    }

    //
    // the other clause
    //

    val group2 = NodeSpec("14%", CountSpec(14, CountModifier.NoModifier, CountUnit.Percentage))
    val moveFrom7 = NodeSpec("Jonglei State", GeoLoc("Jonglei State")) //fixme: didn't find the attachment
    val migration7 = HumanMigrationEdgeSpec(
      group = Some(group2),
      moveFrom = Some(moveFrom7))

    behavior of "migration-originated7"

    passingTest should "have correct group7 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }
    passingTest should "have correct moveFrom7 node" taggedAs (Somebody) in {
      tester.test(moveFrom7) should be (successful)
    }
    passingTest should "have correct migration7 event" taggedAs (Somebody) in {
      tester.test(migration7) should be (successful)
    }


    val moveFrom8 = NodeSpec("Uror", GeoLoc("Uror"))
    val migration8 = HumanMigrationEdgeSpec(
      group = Some(group2),
      moveFrom = Some(moveFrom8))

    behavior of "migration-originated8"

    passingTest should "have correct moveFrom8 node" taggedAs (Somebody) in {
      tester.test(moveFrom8) should be (successful)
    }
    passingTest should "have correct migration8 event" taggedAs (Somebody) in {
      tester.test(migration8) should be (successful)
    }


    val moveFrom9 = NodeSpec("Akobo", GeoLoc("Akobo"))
    val migration9 = HumanMigrationEdgeSpec(
      group = Some(group2),
      moveFrom = Some(moveFrom9))

    behavior of "migration-originated9"

    passingTest should "have correct moveFrom9 node" taggedAs (Somebody) in {
      tester.test(moveFrom9) should be (successful)
    }
    passingTest should "have correct migration9 event" taggedAs (Somebody) in {
      tester.test(migration9) should be (successful)
    }

    val moveFrom10 = NodeSpec("Ayod Counties", GeoLoc("Ayod Counties"))
    val migration10 = HumanMigrationEdgeSpec(
      group = Some(group2),
      moveFrom = Some(moveFrom10))

    behavior of "migration-originated10"

    passingTest should "have correct moveFrom10 node" taggedAs (Somebody) in {
      tester.test(moveFrom10) should be (successful)
    }
    passingTest should "have correct migration10 event" taggedAs (Somebody) in {
      tester.test(migration10) should be (successful)
    }
  }

  {
    val text = "Pagak Reception Centre: As of 28 April 2017, Pagak accommodated around 3,604 new arrivals."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("3,604 new arrivals", CountSpec(3604, CountModifier.Approximate))
    val timeEnd1 = NodeSpec("28 April 2017", TimEx("28 April 2017"))
    val moveTo1 = NodeSpec("Pagak", GeoLoc("Pagak")) // TODO: or is this the intermediate location? //fixme: didn't find the geolocation
    val migration1 = HumanMigrationEdgeSpec(
      group = Some(group1),
      timeEnd = Some(timeEnd1),
      moveTo = Some(moveTo1)
    )

    behavior of "migration-accommodate"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be(successful)
    }
    passingTest should "have correct timeEnd1 node" taggedAs (Somebody) in {
      tester.test(timeEnd1) should be(successful)
    }
    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be(successful)
    }
    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be(successful)
    }
  }

  {
    val text = "Between 1 and 25 March 2017, 13,225 South Sudanese refugees arrived in Gambella, Ethiopia, bringing the total number of new arrivals since September 2016 to 74,825."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("13,225 South Sudanese refugees", CountSpec(13225))
    val moveTo1 = NodeSpec("Gambella", GeoLoc("Gambella"))
    val time1 = NodeSpec("Between 1 and 25 March 2017", TimEx("Between 1 and 25 March 2017"))

    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1), timeStart = Some(time1))


    val group2 = NodeSpec("74,825", CountSpec(74825))
    val timeStart2 = NodeSpec("since September 2016", TimEx("since September 2016"))
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), timeStart = Some(timeStart2), moveTo = Some(moveTo1))

    behavior of "migration-ma-1"

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }

    failingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }

    failingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }

    failingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }

    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }

    passingTest should "have correct timeStart2 node" taggedAs (Somebody) in {
      tester.test(timeStart2) should be (successful)
    }

    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
    }

  }

  {
    val text = "In the past week, the daily arrival average stood at 626 individuals."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("626 individuals", CountSpec(626, CountModifier.NoModifier, CountUnit.Daily))
    val groupModifier = NodeSpec("daily", TimEx("daily"))
    val time1 = NodeSpec("past week", TimEx("past week"))

    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), time = Some(time1), groupModifier = Some(groupModifier)) //fixme: time is not attached => fix rule (migration-arrival or add to migration-rate)


    behavior of "migration-ma-2"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }

    passingTest should "have correct time1 node" taggedAs (Somebody) in {
      tester.test(time1) should be (successful)
    }

    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }
  }

  {
    val text = "Except 1,796 individuals who are awaiting relocation in Pagak, all the new arrivals have been relocated to Nguenyyiel refugee camp."

    val tester = new GraphTester(text)

    val group1 = NodeSpec("1,796 individuals", CountSpec(1796))
    val moveTo1 = NodeSpec("Pagak", GeoLoc("Pagak")) //todo: should this be intermediate point?

    val migration1 = HumanMigrationEdgeSpec(group = Some(group1), moveTo = Some(moveTo1))


    val group2 = NodeSpec("all the new arrivals")
    val moveTo2 = NodeSpec("Nguenyyiel", GeoLoc("Nguenyyiel"))
    val migration2 = HumanMigrationEdgeSpec(group = Some(group2), moveTo = Some(moveTo2))


    behavior of "migration-ma-3"

    passingTest should "have correct group1 node" taggedAs (Somebody) in {
      tester.test(group1) should be (successful)
    }

    passingTest should "have correct moveTo1 node" taggedAs (Somebody) in {
      tester.test(moveTo1) should be (successful)
    }

    passingTest should "have correct migration1 event" taggedAs (Somebody) in {
      tester.test(migration1) should be (successful)
    }


    passingTest should "have correct group2 node" taggedAs (Somebody) in {
      tester.test(group2) should be (successful)
    }

    passingTest should "have correct moveTo2 node" taggedAs (Somebody) in {
      tester.test(moveTo2) should be (successful)
    }

    passingTest should "have correct migration2 event" taggedAs (Somebody) in {
      tester.test(migration2) should be (successful)
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
