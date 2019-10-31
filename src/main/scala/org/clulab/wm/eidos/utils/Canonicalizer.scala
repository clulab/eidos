package org.clulab.wm.eidos.utils

class Canonicalizer(stopwordManaging: StopwordManaging) {

  protected def isContentTag(tag: String): Boolean =
      tag.startsWith("NN") ||
      tag.startsWith("VB") ||
      tag.startsWith("JJ")


  def isCanonical(lemma: String, tag: String, ner: String): Boolean =
    isContentTag(tag) &&
    !stopwordManaging.containsStopword(lemma) &&
    !StopwordManager.STOP_NER.contains(ner)
}
