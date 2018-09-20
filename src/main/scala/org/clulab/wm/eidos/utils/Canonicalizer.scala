package org.clulab.wm.eidos.utils

class Canonicalizer(stopwordManaging: StopwordManaging) {

  protected def isContentTag(tag: String) =
      tag.startsWith("NN") ||
      tag.startsWith("VB")

  protected def removeNER(ner: String) = StopwordManager.STOP_NER.contains(ner)

  def isCanonical(lemma: String, tag: String, ner: String): Boolean =
    isContentTag(tag) &&
    !stopwordManaging.containsStopword(lemma) &&
    !removeNER(ner)
}
