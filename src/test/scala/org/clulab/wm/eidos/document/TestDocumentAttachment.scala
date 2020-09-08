package org.clulab.wm.eidos.document

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.time.LocalDateTime

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.DocumentSerializer
import org.clulab.serialization.json.JSONSerializer
import org.clulab.serialization.json._
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.utils.Closer.AutoCloser
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.attachments.{DctDocumentAttachment, RelevanceDocumentAttachment}
import org.clulab.wm.eidos.test.TestUtils._
import org.json4s.jackson.JsonMethods.{pretty, render}
import org.json4s.jackson.parseJson
import org.json4s.jackson.prettyJson
import org.json4s.jackson.renderJValue

class TestDocumentAttachment extends Test {

  def serialize(any: Any): Array[Byte] = {
    new ByteArrayOutputStream().autoClose { byteArrayOutputStream =>
      new ObjectOutputStream(byteArrayOutputStream).autoClose { objectOutputStream =>
        try {
          objectOutputStream.writeObject(any)
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
      byteArrayOutputStream.toByteArray
    }
  }

  def deserialize[T](byteArray: Array[Byte]): T = {
    new ByteArrayInputStream(byteArray).autoClose { byteArrayInputStream =>
      new ObjectInputStream(byteArrayInputStream).autoClose { objectInputStream =>
        try {
          val res1 = objectInputStream.readObject()
          val res2 = res1.asInstanceOf[T]
          res2
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
    }
  }

  "DctDocumentAttachment" should "serialize" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocumentAttachment = new DctDocumentAttachment(dct)
    val bytes = serialize(oldDocumentAttachment)
    val newDocumentAttachment: DctDocumentAttachment = deserialize(bytes)
    newDocumentAttachment should be (oldDocumentAttachment)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    val oldDocumentAttachment2 = new DctDocumentAttachment(dct2)
    newDocumentAttachment should not be (oldDocumentAttachment2)
  }

  "Document with DctDocumentAttachment" should "serialize as text" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocument = new Document(Array.empty[Sentence])

    DctDocumentAttachment.setDct(oldDocument, dct)

    val documentSerializer = new DocumentSerializer()
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    val newDctOpt = DctDocumentAttachment.getDct(newDocument)
    newDctOpt.get should be (dct)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    newDctOpt.get should not be (dct2)
  }

  "Document with DctDocumentAttachments" should "serialize as json" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocument = new Document(Array.empty[Sentence])

    DctDocumentAttachment.setDct(oldDocument, dct)

    val documentString = prettyJson(renderJValue(oldDocument.jsonAST))

    val newDocument: Document = JSONSerializer.toDocument(parseJson(documentString))
    val newDctOpt = DctDocumentAttachment.getDct(newDocument)
    newDctOpt.get should be (dct)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    newDctOpt.get should not be (dct2)
  }

  "Document with RelevanceDocumentAttachment" should "serialize as text" in {
    val docSentRelevanceScores = Seq(0.7f, 0.8f)
    val oldDocument = new Document(Array.empty[Sentence])

    RelevanceDocumentAttachment.setRelevance(oldDocument, docSentRelevanceScores)

    val documentSerializer = new DocumentSerializer()
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    val newRelevanceOpt = RelevanceDocumentAttachment.getRelevance(newDocument)
    newRelevanceOpt.get.map{x => "%.4f".format(x)}== docSentRelevanceScores.map{x => "%.4f".format(x)} should be (true)
    oldDocument.getAttachment("relevanceScore").equals(newDocument.getAttachment("relevanceScore")) should be (true)


    val docSentRelevanceScores2 = Seq(0.8f, 0.9f, 0.8f)
    newRelevanceOpt.get.map{x => "%.4f".format(x)}== docSentRelevanceScores2.map{x => "%.4f".format(x)} should be (false)
  }

  "Document with RelevanceDocumentAttachments" should "serialize as json" in {
    val docSentRelevanceScores = Seq(0.7f, 0.8f)
    val oldDocument = new Document(Array.empty[Sentence])

    RelevanceDocumentAttachment.setRelevance(oldDocument, docSentRelevanceScores)

    val documentString = prettyJson(renderJValue(oldDocument.jsonAST))

    val newDocument: Document = JSONSerializer.toDocument(parseJson(documentString))
    val newRelevanceOpt = RelevanceDocumentAttachment.getRelevance(newDocument)
    newRelevanceOpt.get.map{x => "%.4f".format(x)}== docSentRelevanceScores.map{x => "%.4f".format(x)} should be (true)
    oldDocument.getAttachment("relevanceScore").equals(newDocument.getAttachment("relevanceScore")) should be (true)


    val docSentRelevanceScores2 = Seq(0.8f, 0.9f, 0.8f)
    newRelevanceOpt.get.map{x => "%.4f".format(x)}== docSentRelevanceScores2.map{x => "%.4f".format(x)} should be (false)
  }


  val config: Config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)
  "Document relevance score added by annotateDoc" should "have 6 non-negative scores" in {

    // This text is randomly selected.
    val docText = "As I wrote about before the conventions, " +
      "all signs pointed to small convention bounces in the Biden-Trump matchup. " +
      "Bounces have been getting smaller in recent years, and the unusual stability of this race, " +
      "well known candidates and low undecideds made it unlikely that the scaled back conventions would produce a large bounce. " +
      "The good news for Biden comes in the form of favorability ratings. " +
      "A new ABC News/Ipsos poll shows that Biden's net favorability rating (favorable - unfavorable) is up compared to before the conventions. " +
      "Stock level and seasonality: according to traders, most commodities are imported, " +
      "with the highest flow of supply observed during the months of january to march, june to july and november to december. " +
      "An additional environmental impact unique to flooded rice systems is an increase in insect-borne disease: flooded rice fields have been associated with an increase in malaria transmission among farmers, workers, and communities adjacent to flooded rice-producing areas in both africa and asia (larson et al. "

    val docObj = eidosSystem.annotate(docText)
    val relevanceScore = RelevanceDocumentAttachment.getRelevance(docObj)
    relevanceScore.get.forall(x => x> -0.1f) should be (true)
    relevanceScore.get.length==6 should be (true)
  }

  // TODO: do we really need this unit test? This is for printing the JSON ouptut.
//  "A sentence" should "print json" in {
//    val docText = "Rainfall can increase poverty. All signs pointed to small convention bounces in the Biden-Trump matchup. "
//    val docAnnotated = eidosSystem.annotate(docText)
//    val serial = pretty(render(docAnnotated.jsonAST))
//
//    println(serial)
//
//    true should be (true)
//
//    // If it needs to be actually written to a file, use FileUtils.write()
//    // http://commons.apache.org/proper/commons-io/apidocs/org/apache/commons/io/FileUtils.html
//
//  }

}
