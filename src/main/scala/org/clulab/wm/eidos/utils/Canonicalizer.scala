package org.clulab.wm.eidos.utils

class Canonicalizer(stopwordManaging: StopwordManaging) {

  protected def isContentTag(tag: String) =
      tag.startsWith("NN") ||
      tag.startsWith("VB")

  def isCanonical(lemma: String, tag: String, ner: String): Boolean =
    isContentTag(tag) &&
    !stopwordManaging.containsStopword(lemma) &&
    !StopwordManager.STOP_NER.contains(ner)
}
