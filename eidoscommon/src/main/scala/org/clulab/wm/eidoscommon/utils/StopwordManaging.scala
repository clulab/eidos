package org.clulab.wm.eidoscommon.utils

trait StopwordManaging {
  def containsStopword(stopword: String): Boolean
  def containsStopwordStrict(stopword: String): Boolean = containsStopword(stopword)
}
