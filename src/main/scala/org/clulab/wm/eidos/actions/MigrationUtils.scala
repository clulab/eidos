package org.clulab.wm.eidos.actions

import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem

object MigrationUtils {
  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)

    // TODO: do
    val handled = migrationEvents
    // todo: add attachments for Time and Location (start with overlapping already found NN ones, see action)
    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????
    // todo: aggregation of cross-sentence stuff?????????????

    // return all
    handled ++ other
  }

  def djasdklj(): Seq[Mention] = ???
}
