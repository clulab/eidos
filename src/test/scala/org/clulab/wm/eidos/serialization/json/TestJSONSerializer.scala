package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json._
import org.clulab.wm.eidos.test.TestUtils.Test

class TestJSONSerializer extends Test {
  
  val reader = new EidosSystem()

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
}
