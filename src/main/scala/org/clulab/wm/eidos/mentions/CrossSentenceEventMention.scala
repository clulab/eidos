package org.clulab.wm.eidos.mentions

import org.clulab.odin.{Attachment, EventMention, Mention, SynPath, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

import scala.collection.mutable.ArrayBuffer

//same as EventMention but with a different foundBy, no paths, sentence == the sent of the trigger, and a different text method
//todo: place elsewhere
//todo: change tokenInterval to something informed and usable
class CrossSentenceEventMention(
                                 labels: Seq[String],
                                 tokenInterval: Interval,
                                 trigger: TextBoundMention,
                                 arguments: Map[String, Seq[Mention]],
                                 paths:Map[String, Map[Mention, SynPath]],
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
    val argsBySent = arguments.values.flatten
      .groupBy(m => m.sentence)
      .toSeq
      .sortBy(_._1) // sort by the sentence
    val textArray = new ArrayBuffer[String]()
    val firstSentIndex = argsBySent.head._1
    val lastSentIndex = argsBySent.last._1
    for ((i, ms) <- argsBySent) {
      val sortedMentions = ms.toList.sortBy(_.startOffset) // sort mentions within the sentence by starting character
      val firstArgInd = sortedMentions.head.start // index of the first token of the first argument of the sentence
      val sentTextArr = sortedMentions.head.sentenceObj.raw //sentence as an array of tokens
      //compile text of the CrossSentenceEventMention from parts of the sentences that the CrossSentenceEventMention spans
      i match { //sentence index (ordered)
        case `firstSentIndex` => {
          //in the first sentence the CrossSentenceEventMention spans, the part to return is the span from the start of the first argument of the event to the end of the sentence
          val relSubString = sentTextArr.slice(firstArgInd, sentTextArr.length).mkString(" ")
          textArray.append(relSubString)
        }
        case `lastSentIndex` => {
          //in the last sentence the CrossSentenceEventMention spans, the part to return is the span from the beginning of the sentence to the end of the last argument
          val relSubString = sentTextArr.slice(0, sortedMentions.last.end).mkString(" ")
          textArray.append(relSubString)
        }
        case _ => textArray.append(sentTextArr.mkString(" ")) //if it's a middle sentence, the part to return is the whole sentence
      }
    }
    val text = textArray.mkString(" ")
    text
  }
}