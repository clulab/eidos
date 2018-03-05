package org.clulab.wm.eidos.serialization.json

import scala.util.hashing.MurmurHash3._

import org.clulab.odin._
import org.clulab.struct.DirectedGraph
import org.clulab.wm.eidos.attachments.EidosAttachment

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.Serialization.write

// Code ported from ODIN

package object json {

  implicit val formats = org.json4s.DefaultFormats

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(MentionOps.jsonAST(_)).toList)
    }
    JObject(args.toList)
  }

  private def attachmentsAST(attachments: Set[Attachment]): Set[JValue] =
    attachments.map(EidosAttachment.asEidosAttachment(_).toJson())

  /** Hash representing the [[Mention.arguments]] */
  private def argsHash(args: Map[String, Seq[Mention]]): Int = {
    val argHashes = for {
      (role, mns) <- args
      bh = stringHash(s"role:$role")
      hs = mns.map(MentionOps.equivalenceHash(_))
    } yield mix(bh, unorderedHash(hs))
    val h0 = stringHash("org.clulab.odin.Mention.arguments")
    finalizeHash(h0, unorderedHash(argHashes))
  }

  private def pathsAST(paths: Map[String, Map[Mention, SynPath]]): JValue = paths match {
    case gps if gps.nonEmpty => OdinPathOps.jsonAST(gps)
    case _ => JNothing
  }


  object MentionOps {

    def jsonAST(m: Mention): JValue = m match {
      case tb: TextBoundMention => TextBoundMentionOps.jsonAST(tb)
      case em: EventMention => EventMentionOps.jsonAST(em)
      case rm: RelationMention => RelationMentionOps.jsonAST(rm)
      case csm: CrossSentenceMention => CrossSentenceMentionOps.jsonAST(csm)
    }

    def stringCode(m: Mention): String = m match {
      case tb: TextBoundMention => TextBoundMentionOps.stringCode
      case em: EventMention => EventMentionOps.stringCode
      case rm: RelationMention => RelationMentionOps.stringCode
      case csm: CrossSentenceMention => CrossSentenceMentionOps.stringCode
    }

    def equivalenceHash(m: Mention): Int = m match {
      case tb: TextBoundMention => TextBoundMentionOps.equivalenceHash(tb)
      case em: EventMention => EventMentionOps.equivalenceHash(em)
      case rm: RelationMention => RelationMentionOps.equivalenceHash(rm)
      case csm: CrossSentenceMention => CrossSentenceMentionOps.equivalenceHash(csm)
    }

    def id(m: Mention): String = m match {
      case tb: TextBoundMention => TextBoundMentionOps.id(tb)
      case em: EventMention => EventMentionOps.id(em)
      case rm: RelationMention => RelationMentionOps.id(rm)
      case csm: CrossSentenceMention => CrossSentenceMentionOps.id(csm)
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    def completeAST(m: Mention): JValue = MentionSeq.jsonAST(Seq(m))

  }

  object TextBoundMentionOps {

    val stringCode = s"org.clulab.odin.${WMTextBoundMention.string}"
    implicit val formats = org.json4s.DefaultFormats


    def equivalenceHash(tb: TextBoundMention): Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, tb.labels.hashCode)
      // interval.start
      val h2 = mix(h1, tb.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, tb.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, tb.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, tb.document.equivalenceHash)
      finalizeHash(h5, 5)
    }

    def id(tb: TextBoundMention): String = s"${WMTextBoundMention.shortString}:${equivalenceHash(tb)}"

    def jsonAST(tb: TextBoundMention): JValue = {
      ("type" -> WMTextBoundMention.string) ~
        // used for correspondence with paths map
        ("id" -> id(tb)) ~
        ("text" -> tb.text) ~
        ("labels" -> tb.labels) ~
        ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
        ("characterStartOffset" -> tb.startOffset) ~
        ("characterEndOffset" -> tb.endOffset) ~
        ("sentence" -> tb.sentence) ~
        ("document" -> tb.document.equivalenceHash.toString) ~
        ("keep" -> tb.keep) ~
        ("foundBy" -> tb.foundBy) ~
        ("attachments" -> attachmentsAST(tb.attachments))
    }
  }

  object EventMentionOps {

    val stringCode = s"org.clulab.odin.${WMEventMention.string}"

    def equivalenceHash(em: EventMention): Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, em.labels.hashCode)
      // interval.start
      val h2 = mix(h1, em.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, em.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, em.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, em.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(em.arguments))
      // trigger
      val h7 = mix(h6, TextBoundMentionOps.equivalenceHash(em.trigger))
      finalizeHash(h7, 7)
    }

    def id(em: EventMention): String = s"${WMEventMention.shortString}:${equivalenceHash(em)}"

    def jsonAST(em: EventMention): JValue = {
      ("type" -> WMEventMention.string) ~
        // used for paths map
        ("id" -> id(em)) ~
        ("text" -> em.text) ~
        ("labels" -> em.labels) ~
        ("trigger" -> TextBoundMentionOps.jsonAST(em.trigger)) ~
        ("arguments" -> argsAST(em.arguments)) ~
        // paths are encoded as (arg name -> (mentionID -> path))
        ("paths" -> pathsAST(em.paths)) ~
        ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
        ("characterStartOffset" -> em.startOffset) ~
        ("characterEndOffset" -> em.endOffset) ~
        ("sentence" -> em.sentence) ~
        ("document" -> em.document.equivalenceHash.toString) ~
        ("keep" -> em.keep) ~
        ("foundBy" -> em.foundBy) ~
        ("attachments" -> attachmentsAST(em.attachments))
    }
  }

  object RelationMentionOps {

    val stringCode = s"org.clulab.odin.${WMRelationMention.string}"

    def equivalenceHash(rm: RelationMention): Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, rm.labels.hashCode)
      // interval.start
      val h2 = mix(h1, rm.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, rm.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, rm.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, rm.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(rm.arguments))
      finalizeHash(h6, 6)
    }

    def id(rm: RelationMention): String = s"${WMRelationMention.shortString}:${equivalenceHash(rm)}"

    def jsonAST(rm: RelationMention): JValue = {
      ("type" -> WMRelationMention.string) ~
        // used for paths map
        ("id" -> id(rm)) ~
        ("text" -> rm.text) ~
        ("labels" -> rm.labels) ~
        ("arguments" -> argsAST(rm.arguments)) ~
        // paths are encoded as (arg name -> (mentionID -> path))
        ("paths" -> pathsAST(rm.paths)) ~
        ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
        ("characterStartOffset" -> rm.startOffset) ~
        ("characterEndOffset" -> rm.endOffset) ~
        ("sentence" -> rm.sentence) ~
        ("document" -> rm.document.equivalenceHash.toString) ~
        ("keep" -> rm.keep) ~
        ("foundBy" -> rm.foundBy) ~
        ("attachments" -> attachmentsAST(rm.attachments))
    }
  }

  object CrossSentenceMentionOps {

    val stringCode = s"org.clulab.odin.${WMCrossSentenceMention.string}"

    def equivalenceHash(csm: CrossSentenceMention): Int = {
      // the seed (not counted in the length of finalizeHash)
      val h0 = stringHash(stringCode)
      // labels
      val h1 = mix(h0, csm.labels.hashCode)
      // interval.start
      val h2 = mix(h1, csm.tokenInterval.start)
      // interval.end
      val h3 = mix(h2, csm.tokenInterval.end)
      // sentence index
      val h4 = mix(h3, csm.sentence)
      // document.equivalenceHash
      val h5 = mix(h4, csm.document.equivalenceHash)
      // args
      val h6 = mix(h5, argsHash(csm.arguments))
      finalizeHash(h6, 6)
    }

    def id(csm: CrossSentenceMention): String = s"${WMCrossSentenceMention.shortString}:${equivalenceHash(csm)}"

    def jsonAST(csm: CrossSentenceMention): JValue = {
      ("type" -> WMCrossSentenceMention.string) ~
        // used for paths map
        ("id" -> id(csm)) ~
        ("text" -> csm.text) ~
        ("labels" -> csm.labels) ~
        ("anchor" -> MentionOps.id(csm.anchor)) ~
        ("neighbor" -> MentionOps.id(csm.anchor)) ~
        ("arguments" -> argsAST(csm.arguments)) ~
        ("tokenInterval" -> Map("start" -> csm.tokenInterval.start, "end" -> csm.tokenInterval.end)) ~
        ("sentence" -> csm.sentence) ~
        ("document" -> csm.document.equivalenceHash.toString) ~
        ("keep" -> csm.keep) ~
        ("foundBy" -> csm.foundBy) ~
        ("attachments" -> attachmentsAST(csm.attachments))
    }
  }

  /** For sequences of mentions */
  object MentionSeq {

    def jsonAST(mentions: Seq[Mention]): JValue = WMJSONSerializer.jsonAST(mentions)

  }

  // Syntactic paths generalized are graph paths
  object OdinPathOps {
    import org.clulab.serialization.json.EdgeOps
    // simplify paths by ignoring Mentions
    def jsonAST(paths: Map[String, Map[Mention, SynPath]]): JValue = {
      val simplePathMap: Map[String, Map[String, List[JValue]]] = paths.mapValues{ innermap =>
        val pairs = for {
          (m: Mention, path: SynPath) <- innermap.toList
          edgeAST = DirectedGraph.triplesToEdges[String](path.toList).map(_.jsonAST)
        } yield (MentionOps.id(m), edgeAST)
        pairs.toMap
      }
      simplePathMap
    }
  }

  object WMTextBoundMention {
    val string = "TextBoundMention"
    val shortString = "T"
  }

  object WMEventMention {
    val string = "EventMention"
    val shortString = "E"
  }

  object WMRelationMention {
    val string = "RelationMention"
    val shortString = "R"
  }

  object WMCrossSentenceMention {
    val string = "CrossSentenceMention"
    val shortString = "CS"
  }
}

