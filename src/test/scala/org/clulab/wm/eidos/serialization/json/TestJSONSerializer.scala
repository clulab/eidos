package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Negation
import org.clulab.wm.eidos.serialization.json._
import org.clulab.wm.eidos.test.TestUtils.Test

class TestJSONSerializer extends Test {
  
  val reader = new EidosSystem()

  behavior of "JSONSerializer"

  it should "serialize and deserialize again" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    val reader = new EidosSystem()
    val annotatedDocument = reader.extractFromText(text)
    // Add some of these just for fun.  This is not necessary if the reader already makes the annotation.
    val negation = new Negation("trigger", Some(Seq("mod1", "mod1")))

    val tmpMentionsOut = annotatedDocument.odinMentions
    val mentionsOut = tmpMentionsOut.map(_.withAttachment(negation))

    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
    val json1 = stringify(jValue1, pretty = true)

    val mentionsIn = WMJSONSerializer.toMentions(jValue1)
    val jValue2 = WMJSONSerializer.jsonAST(mentionsOut)
    val json2 = stringify(jValue2, pretty = true)

    json1 should be (json2)
    mentionsOut should be (mentionsIn)
  }
}
