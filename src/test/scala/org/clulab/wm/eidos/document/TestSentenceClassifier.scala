package org.clulab.wm.eidos.document

import org.clulab.processors.Sentence
import org.clulab.wm.eidos.EidosEnglishProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.Language


// TODO: think of how to add config to this
//class TestSentenceClassifier(ontologyHandler:OntologyHandler) extends Test {
//
//  class TestableSentenceClassifier extends SentenceClassifier(ontologyHandler:OntologyHandler) {
//    // TODO: Put real code into SentenceClassifier, but test code here.
//    var hasBeenCalled = false
//
//    override def classify(sentence: Sentence): Float = {
//      val result = super.classify(sentence)
//
//      hasBeenCalled = true
//      result
//    }
//  }
//
//  def newSentence(words: Array[String]): Sentence = {
//    val startOffsets = words.scanLeft(0) { case (start, word) =>  start + word.length + 1 }.dropRight(1)
//    val endOffsets = words.scanLeft(-1) { case (end, word) => end + 1 + word.length }.drop(1)
//
//    // This may need to be a more complicated sentence with tags, lemmas, etc.
//    new Sentence(words, startOffsets, endOffsets, words)
//  }
//
//  behavior of "SentenceClassifier"
//
//  it should "work independently" in {
//    val sentenceClassifier = new TestableSentenceClassifier()
//    val sentence = newSentence(Array("This", "is", "a", "test", "."))
//
//    sentenceClassifier.classify(sentence)
//    sentenceClassifier.hasBeenCalled should be (true)
//  }
//
//  ignore should "work integrated" in {
//    val sentenceClassifier = new TestableSentenceClassifier()
//    val eidosEnglishProcessor = new EidosEnglishProcessor(Language.ENGLISH, 100)
//
//    eidosEnglishProcessor.extractDocument("This is a test.")
//    sentenceClassifier.hasBeenCalled should be (true)
//  }
//
//  ignore should "work configured" in {
//    val eidosSystem = new EidosSystem(defaultConfig)
//
//    eidosSystem.annotate("This is a test.")
//    // Just don't crash for now.
//    // Later look for side effect in document attachments.
//  }
//}
