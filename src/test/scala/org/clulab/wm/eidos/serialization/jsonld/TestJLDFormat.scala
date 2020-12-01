package org.clulab.wm.eidos.serialization.jsonld

import java.util.{HashMap => JHashMap}

import com.github.jsonldjava.core.{JsonLdOptions, JsonLdProcessor}
import com.github.jsonldjava.utils.JsonUtils
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.test.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._

import scala.collection.Seq

class TestJLDFormat extends ExtractionTest {
  
  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val annotatedDocument = ieSystem.extractFromText(text)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus): String = {
    val jldCorpus = new JLDCorpus(corpus)
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
    val jsonObject = Option(JsonUtils.fromString(json))
        .getOrElse(throw new Exception("jsonObject is empty"))

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
