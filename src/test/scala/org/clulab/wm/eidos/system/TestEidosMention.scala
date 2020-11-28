package org.clulab.wm.eidos.text.english.cag

import java.time.ZonedDateTime
import java.util.HashMap

import org.clulab.odin.Mention
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{EqualityBagger, IdentityBagger}
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.clulab.wm.eidos.utils.OdinMention

class TestEidosMention extends ExtractionTest {
  
  def groundOntology(mention: EidosMention): Map[String, OntologyGrounding] = Map.empty

//  object StopwordManager extends StopwordManaging {
//    def containsStopword(stopword: String) = stopword == "policy"
//  }

  def test(text: String) = {
    def myprintln(text: String) = {
      val debug = false

      if (debug)
        println(text)
    }

    val extractedOdinMentions = ieSystem.extractFromText(text, cagRelevantOnly = false).odinMentions
    val reachableOdinMentions = OdinMention.findAllByEquality(extractedOdinMentions)

    {
      // Diagnostics
      val distinctExtractedOdinMentions = new EqualityBagger[Mention].put(extractedOdinMentions).get
      val uniqueExtractedOdinMentions = new IdentityBagger[Mention].put(extractedOdinMentions).get
      val uniqueDistinctExtractedOdinMentions = new IdentityBagger[Mention].put(distinctExtractedOdinMentions).get

      val distinctReachableOdinMentions = new EqualityBagger[Mention].put(reachableOdinMentions).get
      val uniqueReachableOdinMentions = new IdentityBagger[Mention].put(reachableOdinMentions).get
      val uniqueDistinctReachableOdinMentions = new IdentityBagger[Mention].put(distinctReachableOdinMentions).get

      //    reachableOdinMentions.foreach { odinMention =>
      //      println(System.identityHashCode(odinMention) + "\t" + odinMention.hashCode())
      //    }
    }

    val odinMentions = reachableOdinMentions // These should already be distinct
    val distinctOdinMentions = new EqualityBagger[Mention].put(odinMentions).get // This shouldn't make a difference
    val (eidosMentions, _) = EidosMention.asEidosMentions(odinMentions)
    odinMentions.size should be (distinctOdinMentions.size)
    odinMentions.size should be (eidosMentions.size)

    // Since they are all distinct above, they are also unique
    val odinUniqueMentions = new IdentityBagger[Mention].put(odinMentions).get
    val eidosUniqueMentions = new IdentityBagger[EidosMention].put(eidosMentions).get
    odinMentions.size should be (odinUniqueMentions.size)
    odinMentions.size should be (eidosUniqueMentions.size)

    // These may not be distinct or unique, but their sizes should at least match
    val odinArguments = eidosMentions.flatMap(eidosMention => eidosMention.odinArguments.values).flatten
    val eidosArguments = eidosMentions.flatMap(eidosMention => eidosMention.eidosArguments.values).flatten
    odinArguments.size should be (eidosArguments.size)
    
    myprintln("odinMentions: " + odinMentions.size + " eidosMentions " + eidosMentions.size)
    myprintln("odinArguments: " + odinArguments.size + " eidosArguments " + eidosArguments.size)
  }

  val text1 = """
In the capital, Juba, prices of maize and sorghum more than
doubled in the first semester of 2017, reaching record levels in
June, driven by a tight supply situation, market disruptions,
hyperinflation and a significant depreciation of the local currency.
Subsequently, they declined by about 12 percent between June
and August, following the first season harvest in southern bimodal
rainfall areas and the establishment, by the Government,
of a trading company selling basic food commodities at
subsidized prices. Prices of groundnuts decreased by 22 percent
over the same period, while prices of wheat flour continued to
soar in recent months, reaching new record highs in August.
Overall, prices of these food staples in August were more than
twice the high levels in August last year and up to 12 times higher
than in the corresponding period two years earlier.
      """
  val text2 = "The decrease in rainfall caused significantly increased poverty."
    
  behavior of "EidosMention"
  
  Seq(p1, p2, p3, p4, p5, p6, fullText, text1, text2).zipWithIndex.foreach { case (text, index) =>
    it should "convert text " + index in {
      test(text)
    }
  }

  // Test canonicalFormSimple and canonicalFormArgs
  it should "properly make canonical form" in {
    val text3 = "The seasonal rainfall in July was decreased by the government policy and the price of oil."
    val odinMentions3 = extractMentions(text3)
    val (eidosMentions3, _) = EidosMention.asEidosMentions(odinMentions3)
    val canonicalizer = ieSystem.components.ontologyHandlerOpt.get.canonicalizer

    val rainfall = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July")
    rainfall should have size(1)
    rainfall.head.canonicalName = canonicalizer.canonicalize(rainfall.head)
    rainfall.head.canonicalName should be ("seasonal rainfall")

    val decrease = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July was decreased by the government policy")
    decrease should have size(1)
    decrease.head.canonicalName = canonicalizer.canonicalize(decrease.head)
    decrease.head.canonicalName should be ("seasonal rainfall decreased government policy")

    // Since we filter out the text from attachments, "price" should be removed (Property attachment)
    val oil = eidosMentions3.filter(m => m.odinMention.text == "price of oil")
    oil should have size(1)
    oil.head.canonicalName = canonicalizer.canonicalize(oil.head)
    canonicalizer.canonicalize(oil.head) should be ("oil")
  }

  behavior of "mentions resulting from reading"

  // It should also have the triggers, of course.
  it should "include all the argument mentions" in {
    val text = "85% of the new arrivals originated from Upper Nile State (Nasir, Longechuk or Mathiang, Ulang and Maiwut Counties), whilst 14% came from Jonglei State (Uror, Akobo and Ayod Counties)."
    val odinMentions = ieSystem.extractFromText(text).odinMentions
    val reachableOdinMentions = OdinMention.findAllByEquality(odinMentions)

    odinMentions.foreach { odinMention =>
      val argumentMentions = odinMention.arguments.values.flatten

      argumentMentions.foreach { argumentMention =>
//        if (!odinMentions.exists { odinMention => odinMention.eq(argumentMention) })
//          println("I couldn't find $argumentMention by eq in odinMentions!")
//        if (!reachableOdinMentions.exists { odinMention => odinMention.eq(argumentMention) })
//          println("I couldn't find $argumentMention by eq in reachableOdinMentions!")
//        if (!reachableOdinMentions.exists { odinMention => odinMention.equals(argumentMention) })
//          println("I couldn't find $argumentMention by equals in reachableOdinMentions!")
        reachableOdinMentions should contain (argumentMention)
      }
    }
  }
}
