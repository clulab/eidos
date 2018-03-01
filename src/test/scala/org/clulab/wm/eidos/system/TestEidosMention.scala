package org.clulab.wm.eidos.text.cag

import java.util.IdentityHashMap  // Unfortunately borrowed from Java

import org.clulab.odin.Mention
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.cag.CAG._

class TestEidosMention extends Test {
  
  def test(text: String) = {
    def myprintln(text: String) = {
      val debug = false
      
      if (debug)
        println(text)
    }
      
    def addAllOdinMention(map: IdentityHashMap[Mention, Int], values: Seq[Mention]): Unit =
        values.foreach { value =>
          if (map.containsKey(value))
            map.put(value, map.get(value) + 1)
          else
            map.put(value, 1)
        }
      
    def addAllEidosMention(map: IdentityHashMap[EidosMention, Int], values: Seq[EidosMention]): Unit =
        values.foreach { value =>
          if (map.containsKey(value))
            map.put(value, map.get(value) + 1)
          else
            map.put(value, 1)
        }

    val odinMentions = TestUtils.extractMentions(text)
    val eidosMentions = EidosMention.asEidosMentions(odinMentions)
    val mentionsSize = odinMentions.size
    
    myprintln("mentionsSize: " + mentionsSize)
    odinMentions.size should be (eidosMentions.size)
    
    val odinArguments = eidosMentions.flatMap(eidosMention => eidosMention.odinArguments.values).flatten
    val eidosArguments = eidosMentions.flatMap(eidosMention => eidosMention.eidosArguments.values).flatten
    val argumentsSize = odinArguments.size
    
    myprintln("argumentsSize: " + argumentsSize)
    odinArguments.size should be (eidosArguments.size)
    
    // Collect unique values?
    myprintln("odinMentions: " + odinMentions.size + " eidosMentions " + eidosMentions.size)
    myprintln("odinArguments: " + odinArguments.size + " eidosArguments " + eidosArguments.size)
    
    val odinUniqueMentions = new IdentityHashMap[Mention, Int]()
    val eidosUniqueMentions = new IdentityHashMap[EidosMention, Int]()
    
    addAllOdinMention(odinUniqueMentions, odinMentions)
    addAllEidosMention(eidosUniqueMentions, eidosMentions)
    val uniqueSize1 = odinUniqueMentions.size()
    
    
    myprintln("uniqueSize1: " + uniqueSize1)
    odinUniqueMentions.size should be (eidosUniqueMentions.size)
    
    addAllOdinMention(odinUniqueMentions, odinArguments)
    addAllEidosMention(eidosUniqueMentions, eidosArguments)
    val uniqueSize2 = odinUniqueMentions.size()

    myprintln("uniqueSize2: " + uniqueSize2)
    odinUniqueMentions.size should be (eidosUniqueMentions.size)
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
    it should "should convert text " + index in {
      test(text)
    }
  }

  // Test canonicalFormSimple and canonicalFormArgs
  it should "properly make canonical form" in {
    val text3 = "The seasonal rainfall in July was decreased by something."
    val odinMentions3 = TestUtils.extractMentions(text3)
    val eidosMentions3 = EidosMention.asEidosMentions(odinMentions3)

    //  eidosMentions3.foreach(m => println(s"\t${m.odinMention.text}\tcanonical: ${m.canonicalName}"))

    val rainfall = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July")
    rainfall should have size(1)
    rainfall.head.canonicalName should be ("rainfall July")

    val decrease = eidosMentions3.filter(m => m.odinMention.text == "seasonal rainfall in July was decreased by something")
    decrease should have size(1)
    decrease.head.canonicalName should be ("rainfall July decrease something")

  }


}
