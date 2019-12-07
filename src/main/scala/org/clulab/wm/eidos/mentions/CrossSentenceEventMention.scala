package org.clulab.wm.eidos.mentions

import org.clulab.odin.{Attachment, EventMention, Mention, SynPath, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

//same as EventMention but with a different foundBy, no paths, sentence == the sent of the trigger, and a different text method
//todo: place elsewhere
//todo: change tokenInterval to something informed and usable
//TODO: These will be serialized as EventMentions, and then won't be deserialized properly to this subclass.
class CrossSentenceEventMention(
  labels: Seq[String],
  tokenInterval: Interval,
  trigger: TextBoundMention,
  arguments: Map[String, Seq[Mention]],
  paths: Map[String, Map[Mention, SynPath]],
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String,
  attachments: Set[Attachment] = Set.empty
) extends EventMention(labels, tokenInterval, trigger, arguments, Map.empty, trigger.sentence, document, keep, foundBy, attachments) {

  //the text method is overridden bc the EventMention text method does not work with cross sentence mentions
  //todo: is the text of a mention the arg span or the trigger span should also be included (in cases when all the args are to one side of the trigger)?
  //todo: now, the sent/clause in between is not returned, e.g., in 440 people left France for Romania. 300 refugees fled South Sudan; they left the country for Ethiopia. They left in 1997.
  override def text: String = {
    val sentenceAndMentionsSeq = arguments
        .values
        .flatten
        .groupBy(_.sentence)
        .toSeq
        .sortBy(_._1) // sort by the sentence
    // Since this is CrossSentence, there are at least two different sentences.
    val firstSentence = sentenceAndMentionsSeq.head._1
    val lastSentence = sentenceAndMentionsSeq.last._1
    val perSentenceWords = sentenceAndMentionsSeq.map { case (sentence, mentions) =>
      val sentenceWordArr = mentions.head.sentenceObj.raw //sentence as an array of tokens
      //compile text of the CrossSentenceEventMention from parts of the sentences that the CrossSentenceEventMention spans
      val words = sentence match { //sentence index (ordered)
        case sentence if sentence == firstSentence =>
          // in the first sentence the CrossSentenceEventMention spans, the part to return is the span from the start of the first argument of the event to the end of the sentence
          // Although it doesn't matter much with a small collection, sorting one in its entirety just to extract
          // an extreme value is inefficient.  So, a simple minBy is used.  Is there no minByBy?
          val start = mentions.minBy(_.start).start
          sentenceWordArr.drop(start)
        case sentence if sentence == lastSentence =>
          // in the last sentence the CrossSentenceEventMention spans, the part to return is the span from the beginning of the sentence to the end of the last argument
          // Although it may not be a problem in this context, the maximum end does not necessarily come from the
          // mention with the maximum start.  Sometimes mentions overlap and they might conceivably be nested.
          val end = mentions.maxBy(_.end).end
          sentenceWordArr.take(end)
        case _ =>
          // if it's a middle sentence, the part to return is the whole sentence
          sentenceWordArr
      }

      words
    }
    val text = perSentenceWords.flatten.mkString(" ")

    text
  }
}