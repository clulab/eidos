package org.clulab.wm.eidos.actions

import org.clulab.odin.{EventMention, Mention, State, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.EidosDocument

object MigrationUtils {
  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)

    // TODO: do

//    val handled = migrationEvents
    // todo: add attachments for Time and Location (start with overlapping already found NN ones, see action)
//     def applyLocationAttachmentMigration(ms: Seq[Mention]): Seq[Mention] = {
//       for {
//         m <- ms
// //        trigger = m.asInstanceOf[EventMention].trigger
// //        locations = Set("moveTo", "moveFrom", "moveThrough")
//         locationMention = m.arguments("moveTo").head
// //        theme = tieBreaker(m.arguments("theme")).asInstanceOf[TextBoundMention]
//         geolocs = m.document.asInstanceOf[EidosDocument].geolocs
//         location: Option[GeoPhraseID] = if (geolocs.isDefined) geolocs.get(m.sentence).find(_.startOffset == locationMention.startOffset) else None
//         locationAttachment = if (location.isEmpty) locationMention else locationMention.withAttachment(new Location(location.head))
//         } yield m.
// //        })
// //      } yield m.arguments ++ (location match {
// //        case None => locationMention
// //        case Some(l) => locationMention.withAttachment(new Location(l))
// //      })
//     }
    // w/ locationMention, we get correct attachment to Location but lose humanMigration event
    // copy copyWithnewArgs from ArgumentExpander.scala in Eidos

    // val handled = applyLocationAttachmentMigration(migrationEvents)

    val handled = migrationEvents


    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????
    // todo: aggregation of cross-sentence stuff?????????????

    // return all
    handled ++ other
  }

  def djasdklj(): Seq[Mention] = ???
}
