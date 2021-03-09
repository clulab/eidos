package org.clulab.wm.eidos.test

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.graph
import org.clulab.wm.eidos.rule
import org.clulab.wm.eidos.test.TestUtils.newEidosSystem
import org.clulab.wm.eidoscommon.utils.Test

import scala.collection.Seq

class EidosTest extends Test {
  val defaultConfig: Config = ConfigFactory.load(EidosTest.config)

  val passingTest = it
  val failingTest = ignore
  val brokenSyntaxTest = ignore
  val futureWorkTest = ignore // added to mark the tests that are not currently passing, but with planned changes to the
  // framework, they will be achievable
  val inferenceTest = ignore  // type of futureWorkTest -- added for tests which are now failing because of entity
  // filtering, basically because inference or coref would be needed
  val tempBrokenEntitiesTest = ignore
  val affectEventTest = ignore
  val waitingForProcessors = ignore  // type of futureWorkTest -- added for tests which are now failing because they where designed using a SNAPSHOT version of processors

  // This is usually disabled.
  // val longTimeNormTest = it
  val longTimeNormTest = ignore
}

object EidosTest {
  val config = "englishTest" // CLU Lab version
}

class ContraptionTest extends EidosTest

class ExtractionTest(val ieSystem: EidosSystem, val config: Config) extends ContraptionTest {
  // These multiple configs are to ensure that the same config that was used to initialize the EidosSystem
  // is available to the test framework without EidosSystem needing to record it in some way.
  def this(config: Config = ConfigFactory.load(EidosTest.config)) = this(newEidosSystem(config), config)

  class GraphTester(text: String) extends graph.GraphTester(ieSystem, text)

  class RuleTester(text: String) extends rule.RuleTester(ieSystem, text)

  def useTimeNorm = ieSystem.components.useTimeNorm
  def useGeoNorm = ieSystem.components.useGeoNorm

  def extractMentions(text: String): Seq[Mention] = TestUtils.extractMentions(ieSystem, text)
}

class EnglishTest(ieSystem: EidosSystem, config: Config) extends ExtractionTest(ieSystem, config) {
  def this(config: Config = ConfigFactory.load(EidosTest.config)) = this(newEidosSystem(config), config)
}

class PortugueseTest(ieSystem: EidosSystem, config: Config) extends ExtractionTest(ieSystem, config) {
  def this(config: Config = ConfigFactory.load("portugueseTest")) = this(newEidosSystem(config), config)
}
