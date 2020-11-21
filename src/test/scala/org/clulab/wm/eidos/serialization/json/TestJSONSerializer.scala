package org.clulab.wm.eidos.serialization.json

import java.time.LocalDateTime

import org.clulab.serialization.json.stringify
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.{DCT, GeoPhraseID, TimEx, TimeStep}
import org.clulab.wm.eidos.test.TestUtils.Test
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

class TestJSONSerializer extends Test {
  val config = this.defaultConfig
  val reader = new EidosSystem(config)

  behavior of "JSONSerializer"

  {
    val text = "Water trucking has decreased due to the cost of fuel."

    it should s"""serialize "$text"""" in {
      val annotatedDocument = reader.extractFromText(text)

      val mentionsOut = annotatedDocument.odinMentions
      val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
      val json1 = stringify(jValue1, pretty = true)

      val mentionsIn = WMJSONSerializer.toMentions(jValue1)
      val jValue2 = WMJSONSerializer.jsonAST(mentionsOut)
      val json2 = stringify(jValue2, pretty = true)

      json1 should be(json2)
      mentionsOut should be(mentionsIn)
    }
  }

  {
    // This text is especially here for the CrossSentenceEventMention.
    val text = "300 refugees fled South Sudan; they left the country for Ethiopia. They left in 1997."

    it should s"""serialize "$text"""" in {
      val annotatedDocument = reader.extractFromText(text)

      val mentionsOut = annotatedDocument.odinMentions
      val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
      val json1 = stringify(jValue1, pretty = true)

      val mentionsIn = WMJSONSerializer.toMentions(jValue1)
      val jValue2 = WMJSONSerializer.jsonAST(mentionsOut)
      val json2 = stringify(jValue2, pretty = true)

      json1 should be(json2)
      mentionsOut should be(mentionsIn)
    }
  }

  {
    def testAttachment(eidosAttachment: EidosAttachment): Unit = {

      it should s"serialize a(n) ${eidosAttachment.getClass.getSimpleName}" in {

        def toJson(eidosAttachment: EidosAttachment) = {
          val jValue = eidosAttachment.toJson
          val json = stringify(jValue, pretty = true)

          json
        }

        def fromJson(json: String): EidosAttachment = {
          val jValue: JValue = JsonMethods.parse(json)
          val eidosAttachment = EidosAttachment.newEidosAttachment(jValue)

          eidosAttachment
        }

        val json = toJson(eidosAttachment)
        val copy = fromJson(json)

        eidosAttachment should be(copy)
      }
    }

    val trigger = "trigger"
    val someQuantifications = Some(Seq("one", "two"))
    val geoPhraseID = GeoPhraseID("text", Some("Denmark"), 3, 5)
    val timEx = TimEx(TextInterval(3, 8), Seq(TimeStep(LocalDateTime.now, LocalDateTime.now.plusDays(1))), "text")
    val dct = DCT(SimpleInterval(LocalDateTime.now.minusHours(5), LocalDateTime.now), "text")
    val attachments = Seq(
      new Decrease(trigger, someQuantifications),
      new Increase(trigger, someQuantifications),
      new Quantification(trigger, someQuantifications),
      new Property(trigger, someQuantifications),
      new Hedging(trigger, someQuantifications),
      new Negation(trigger, someQuantifications),
      new PosChange(trigger, someQuantifications),
      new NegChange(trigger, someQuantifications),

      new Location(geoPhraseID),
      new Time(timEx),
      new DCTime(dct),
      new Score(4.5d)
    )

    attachments.foreach(testAttachment)
  }
}
