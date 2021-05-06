package org.clulab.wm.discovery

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class TestConceptDiscovery extends AnyFlatSpec with Matchers {
  {
    behavior of "concept discovery"

    // Create two files from https://en.wikipedia.org/wiki/Food_security
    val paths = for (text <- Seq(
      """
        |Food security is a measure of the availability of food and individuals' ability to access it. According the
        |United Nations’ Committee on World Food Security, food security is defined as the means that all people, at
        |all times, have physical, social, and economic access to sufficient, safe, and nutritious food that meets
        |their food preferences and dietary needs for an active and healthy life.[1] The availability of food
        |irrespective of class, gender or region is another one. There is evidence of food security being a concern
        |many thousands of years ago, with central authorities in ancient China and ancient Egypt being known to
        |release food from storage in times of famine. At the 1974 World Food Conference the term "food security"
        |was defined with an emphasis on supply; food security is defined as the "availability at all times of
        |adequate, nourishing, diverse, balanced and moderate world food supplies of basic foodstuffs to sustain a
        |steady expansion of food consumption and to offset fluctuations in production and prices".[2] Later
        |definitions added demand and access issues to the definition. The final report of the 1996 World Food Summit
        |states that food security "exists when all people, at all times, have physical and economic access to
        |sufficient, safe and nutritious food to meet their dietary needs and food preferences for an active and
        |healthy life."[3][4]
        |""".stripMargin,
      """
        |Household food security exists when all members, at all times, have access to enough food for an active,
        |healthy life.[5] Individuals who are food secure do not live in hunger or fear of starvation.[6] Food
        |insecurity, on the other hand, is defined by the United States Department of Agriculture (USDA) as a
        |situation of "limited or uncertain availability of nutritionally adequate and safe foods or limited or
        |uncertain ability to acquire acceptable foods in socially acceptable ways".[7] Food security incorporates a
        |measure of resilience to future disruption or unavailability of critical food supply due to various risk
        |factors including droughts, shipping disruptions, fuel shortages, economic instability, and wars. In the
        |years 2011–2013, an estimated 842 million people were suffering from chronic hunger.[8] The Food and
        |Agriculture Organization of the United Nations, or FAO, identified the four pillars of food security as
        |availability, access, utilization, and stability.[9] The United Nations (UN) recognized the Right to Food in
        |the Declaration of Human Rights in 1948,[6] and has since said that it is vital for the enjoyment of all
        |other rights.[10]
        |""".stripMargin
    )) yield {
      val path = Files.createTempFile(this.getClass.getSimpleName, "test")
      Files.write(path, text.getBytes("utf-8"))
      path
    }

    val conceptDiscovery = new ConceptDiscovery
    val concepts = conceptDiscovery.discoverConcepts(paths)

    it should "find food security concepts" in {
      concepts.map(_.phrase) should contain allOf("food security", "access", "availability")
    }

    it should "have reasonable frequency estimates" in {
      concepts.foreach{
        case Concept("food security", frequency, _) => frequency should be > 4 // actual: 7
        case Concept("access", frequency, _) => frequency should be > 2 // actual: 4
        case Concept("availability", frequency, _) => frequency should be > 2 // actual: 4
        case _ =>
      }
    }
  }

}
