package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test

class TestJSONSerializer extends Test {
  
  val reader = new EidosSystem()

  behavior of "JSONSerializer"

  ignore should "serialize and deserialize again" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    val annotatedDocument = reader.extractFrom(text)
    val mentionsOut = annotatedDocument.mentions
    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
    val mentionsIn = WMJSONSerializer.toMentions(jValue1)
    val jValue2 = WMJSONSerializer.jsonAST(mentionsOut)
    val json1 = stringify(jValue1, pretty = true)
    val json2 = stringify(jValue2, pretty = true)

//    println(json1)
//    println(json2)

    json1 should be (json2)
//    mentionsOut should be (mentionsIn)
  }
}
