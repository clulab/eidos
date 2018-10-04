package org.clulab.wm.eidos.text.cag

import java.util.HashMap

import org.clulab.odin.Mention
import org.clulab.wm.eidos.groundings.{MultiOntologyGrounder, OntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.mentions.{HashCodeBagger, IdentityBagger}
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.cag.CAG._
import org.clulab.wm.eidos.utils.{Canonicalizer, StopwordManaging}

class TestEidosMention extends Test with StopwordManaging with MultiOntologyGrounder {
  
  def groundOntology(mention: EidosMention): Map[String, OntologyGrounding] = Map.empty
  def containsStopword(stopword: String) = stopword == "policy"

  def test(text: String) = {
    def myprintln(text: String) = {
      val debug = false

      if (debug)
        println(text)
    }

    val extractedOdinMentions = ieSystem.extractFromText(text, cagRelevantOnly = false).odinMentions
    val reachableOdinMentions = EidosMention.findReachableMentions(extractedOdinMentions)

    {
      // Diagnostics
      val distinctExtractedOdinMentions = new HashCodeBagger[Mention].put(extractedOdinMentions).get()
      val uniqueExtractedOdinMentions = new IdentityBagger[Mention].put(extractedOdinMentions).get()
      val uniqueDistinctExtractedOdinMentions = new IdentityBagger[Mention].put(distinctExtractedOdinMentions).get()

      val distinctReachableOdinMentions = new HashCodeBagger[Mention].put(reachableOdinMentions).get()
      val uniqueReachableOdinMentions = new IdentityBagger[Mention].put(reachableOdinMentions).get()
      val uniqueDistinctReachableOdinMentions = new IdentityBagger[Mention].put(distinctReachableOdinMentions).get()

      //    reachableOdinMentions.foreach { odinMention =>
      //      println(System.identityHashCode(odinMention) + "\t" + odinMention.hashCode())
      //    }
    }

    val odinMentions = reachableOdinMentions // These should already be distinct
    val distinctOdinMentions = new HashCodeBagger[Mention].put(odinMentions).get() // This shouldn't make a difference
    val eidosMentions = EidosMention.asEidosMentions(odinMentions, new Canonicalizer(this), this)
    odinMentions.size should be (distinctOdinMentions.size)
    odinMentions.size should be (eidosMentions.size)

    // Since they are all distinct above, they are also unique
    val odinUniqueMentions = new IdentityBagger[Mention].put(odinMentions).get()
    val eidosUniqueMentions = new IdentityBagger[EidosMention].put(eidosMentions).get()
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
    val text3 = "The seasonal rainfall in July was decreased by the government policy."
    val odinMentions3 = TestUtils.extractMentions(text3)
    val eidosMentions3 = EidosMention.asEidosMentions(odinMentions3, new Canonicalizer(this), this)

//    eidosMentions3.foreach(m => println(s"\t${m.odinMention.text}\tcanonical: ${m.canonicalName}"))

    val rainfall = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July")
    rainfall should have size(1)
    rainfall.head.canonicalName should be ("rainfall")

    val decrease = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July was decreased by the government policy")
    decrease should have size(1)
    decrease.head.canonicalName should be ("rainfall decrease government")

  }


}
