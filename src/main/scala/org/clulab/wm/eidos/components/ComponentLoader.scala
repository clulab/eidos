package org.clulab.wm.eidos.components

import org.clulab.wm.eidoscommon.utils.Timer

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class ComponentLoader[I, T](val name: String, opt: Boolean, oldLoadedOpt: Option[T], initializer: => I, loader: I => T)
    (implicit parallel: Boolean) {
  import scala.concurrent.ExecutionContext.Implicits.global
  // The lazy is so that nothing begins before the timer is started.
  protected lazy val initializedOpt: Option[Future[I]] =
  // The result of the previous initialization is not saved, so if we are reloading,
  // initialized is not recalculated.  Return None in that case.
    if (!opt || oldLoadedOpt.isDefined)
      None
    else {
      val initializing = Future {
        Timer.time(s"Init $name", ComponentLoader.useTimer) {
          initializer
        }
      }
      if (!parallel) Await.result(initializing, Duration.Inf) // If it isn't parallel, wait for the result.
      Some(initializing)
    }
  lazy val loaded: Future[Option[T]] =
    if (!opt)
      Future {
        None
      }
    else
      oldLoadedOpt
          .map { oldLoaded =>
            Future {
              // If there is an old value available, use it.
              Timer.time(s"Reload $name", ComponentLoader.useTimer) {
                Some(oldLoaded)
              }
            }
          }
          .getOrElse {
            val initialized = initializedOpt.get
            val loading = initialized.map { initialized =>
              Timer.time(s"Load $name", ComponentLoader.useTimer) {
                Some(loader(initialized))
              }
            }
            if (!parallel) Await.result(loading, Duration.Inf) // If it isn't parallel, wait for the result.
            loading
          }

  protected def await: Option[T] = Await.result(loaded, Duration.Inf)

  // It is assumed that opt is true here.
  def get: T = await.get

  def getOpt: Option[T] = await
}

object ComponentLoader {
  var useTimer = true
}
