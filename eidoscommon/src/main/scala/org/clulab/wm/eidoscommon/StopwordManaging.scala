package org.clulab.wm.eidoscommon

trait StopwordManaging {
  def containsStopword(stopword: String): Boolean

  def containsStopwordStrict(stopword: String): Boolean = containsStopword(stopword)
}
