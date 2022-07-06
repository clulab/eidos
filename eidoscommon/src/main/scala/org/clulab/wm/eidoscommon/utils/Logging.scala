package org.clulab.wm.eidoscommon.utils

import com.typesafe.scalalogging.Logger

// The processors version of this returns an slf4j Logger.
// However, here I would like lazy evaluation of the
// arguments to the logging methods (info, trace, etc.),
// so the Scala version is favored.  The reason that
// LazyLogging is not being used is that the logger
// there is marked protected so that other related classes
// can't use it.  Here, a Scala logger is manually created.
trait Logging {
  lazy val logger: Logger = Logger(this.getClass)
}
