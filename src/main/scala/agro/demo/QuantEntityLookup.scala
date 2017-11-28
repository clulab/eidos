//package agro.demo
//
//import org.clulab.processors.bionlp.ner._
//import ai.lum.common.ConfigUtils._
//import com.typesafe.config.ConfigFactory
//import com.typesafe.scalalogging.LazyLogging
//import org.clulab.processors.Document
//import org.clulab.processors.Sentence
//import scala.collection.mutable.ArrayBuffer
//import org.clulab.processors.fastnlp.FastNLPProcessor
//
//class QuantEntityLookup(quantKB: String) {
//  val RuleNER = ??? //KBLoader.load(List(quantKB))
//}
//
//object QuantEntityLookup extends App with LazyLogging {
//
//    /** generates character offsets for the given array of words */
//  def mkOffsets(words: Array[String]): (Array[Int], Array[Int]) = {
//    var start: Int = 0
//    var end: Int = words.head.size
//    val startOffsets = new ArrayBuffer[Int]
//    val endOffsets = new ArrayBuffer[Int]
//    // add first word
//    startOffsets += start
//    endOffsets += end
//    // iterate over all words except first one
//    for (w <- words.tail) {
//      start = end + 1 // plus a space
//      end = start + w.size
//      startOffsets += start
//      endOffsets += end
//    }
//    (startOffsets.toArray, endOffsets.toArray)
//  }
//
//  def annotate(doc:Document) {
//    proc.tagPartsOfSpeech(doc)
//    proc.lemmatize(doc)
////    proc.recognizeNamedEntities(doc)
////    doc.clear()
//  }
//
//  val config = ConfigFactory.load()         // load the configuration file
//
//  val QUANTIFIER_KB: String = config[String]("wmseed.quantifierKB")
//  logger.info("Quantifier KB: " + QUANTIFIER_KB)
//
//  val quantEntityLookup = new QuantEntityLookup(QUANTIFIER_KB)
//  val ruleNER = quantEntityLookup.RuleNER
//  logger.info("Loading Quantifier KB")
//
//  val proc = new FastNLPProcessor
//  val doc = proc.mkDocument("This is some test sentence. This is a much longer sentence compared to the previous sentence. This is a simple sentence.")
//  annotate(doc)
//
////  val s1 = "This is some test sentence"
////  val s2 = "This is a much longer sentence compared to the previous sentence"
////  val offset1 = mkOffsets(s1.split(" "))
////  val sent1 = new Sentence(s1.split(" "), offset1._1, offset1._2)
////  val offset2 = mkOffsets(s2.split(" "))
////  val sent2 = new Sentence(s2.split(" "), offset2._1, offset2._2)
//
////  val sents = Array(sent1, sent2)
////  println(s"SZ : ${sents.size}")
////  val doc: Document = new Document( sents )
////
//  for(sentence <- doc.sentences) {
//    sentence.entities = Some(ruleNER.find(sentence))
//    logger.info(s"${ruleNER.find(sentence).mkString("\t")}")
//  }
//
//  for (sentence <- doc.sentences) {
//    logger.info(s"Sentence: ${sentence.words.mkString(" ")}")
//    if(sentence.entities.nonEmpty) logger.info(s"Match is given by ${sentence.entities.get.mkString(" ")}")
//  }
//}