package org.clulab.wm.eidos.serialization.json

import java.util.{HashMap => JHashMap}

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.AnnotatedDocument
import org.clulab.wm.eidos.EidosSystem.Corpus
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.text.cag.CAG._
import com.github.jsonldjava.core.JsonLdOptions
import com.github.jsonldjava.core.JsonLdProcessor
import com.github.jsonldjava.utils.JsonUtils

import scala.collection.Seq

class TestJLDFormat extends Test {
  
  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val ieSystem = TestUtils.ieSystem
    val annotatedDocument = ieSystem.extractFromText(text)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus): String = {
    val jldCorpus = new JLDCorpus(corpus, TestUtils.ieSystem)
    val jValue = jldCorpus.serialize()
    stringify(jValue, pretty = true)
  }

  behavior of "JLDSerializer"

  it should "serialize in a readable way" in {
    val json = serialize(Seq(newTitledAnnotatedDocument(p1, "p1")))
    json should not be empty

    // See https://github.com/jsonld-java/jsonld-java
    // Read the string into an Object (The type of this object will be a List, Map, String, Boolean,
    // Number or null depending on the root object in the file).
    val jsonObject = JsonUtils.fromString(json)
    // Create a context JSON map containing prefixes and definitions
    val context = new JHashMap()
    // Customise context...
    // Create an instance of JsonLdOptions with the standard JSON-LD options
    val options = new JsonLdOptions
    // Customise options...

    // Call whichever JSONLD function you want! (e.g. compact)
    val compact = JsonUtils.toPrettyString(JsonLdProcessor.compact(jsonObject, context, options))
    compact should not be empty

    val expand = JsonUtils.toPrettyString(JsonLdProcessor.expand(jsonObject))
    expand should not be empty

    val flatten = JsonUtils.toPrettyString(JsonLdProcessor.flatten(jsonObject, options))
    flatten should not be empty

    val normalize = JsonUtils.toPrettyString(JsonLdProcessor.normalize(jsonObject))
    normalize should not be empty
  }
}
