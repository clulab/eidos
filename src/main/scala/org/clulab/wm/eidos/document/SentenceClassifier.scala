package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.utils.Language

class SentenceClassifier {

  def classify(sentence: Sentence): Float = {
    1.0f
  }
}

object SentenceClassifier {

  def fromConfig(config: Config, language: String): Option[SentenceClassifier] = {
    if (language == Language.ENGLISH)
      Some(new SentenceClassifier()) // TODO: Use any config settings necessary
    else
      None
  }
}
