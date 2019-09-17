package org.clulab.wm.eidos.utils

import org.clulab.odin.Mention
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.mentions.EidosMention

class Canonicalizer(stopwordManaging: StopwordManaging) {

  protected def isContentTag(tag: String): Boolean =
      tag.startsWith("NN") ||
      tag.startsWith("VB")

  def isCanonical(lemma: String, tag: String, ner: String): Boolean =
      isContentTag(tag) &&
      !stopwordManaging.containsStopword(lemma) &&
      !StopwordManager.STOP_NER.contains(ner)

  // This is the filtering method for deciding what makes it into the canonical name and what doesn't.
  def canonicalTokensSimple(odinMention: Mention): Seq[String] = {
    val words = odinMention.words
    val lemmas = odinMention.lemmas.get
    val tags = odinMention.tags.get
    val ners = odinMention.entities.get

    val attachmentWords = odinMention.attachments.flatMap(a => EidosAttachment.getAttachmentWords(a))

    val contentLemmas = for {
      i <- lemmas.indices
      if isCanonical(lemmas(i), tags(i), ners(i))
      if !attachmentWords.contains(words(i))
    } yield lemmas(i)

    if (contentLemmas.isEmpty)
      words   // fixme -- better and cleaner backoff
    else
      contentLemmas
  }

  /**
   * To handle mentions that span multiple sentences, we sort the pieces of the mention and then filter each
   * to get the tokens that will make it into the canonicalName.
   */
  def canonicalNameParts(eidosMention: EidosMention): Array[String] = {
    // Sentence has been added to account for cross sentence mentions.
    def lessThan(left: Mention, right: Mention): Boolean =
      if (left.sentence != right.sentence)
        left.sentence < right.sentence
      else if (left.start != right.start)
        left.start < right.start
      // This one shouldn't really be necessary.
      else if (left.end != right.end)
        left.end < right.end
      else
        false // False is needed to preserve order on tie.

    eidosMention.canonicalMentions.sortWith(lessThan).flatMap(canonicalTokensSimple).toArray
  }

  def canonicalize(eidosMention: EidosMention): Unit =
      eidosMention.canonicalName = Some(canonicalNameParts(eidosMention).mkString(" "))
}
