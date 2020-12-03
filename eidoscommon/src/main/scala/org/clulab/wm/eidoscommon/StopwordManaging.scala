package org.clulab.wm.eidoscommon

trait StopwordManaging {
  def containsStopwordNer(stopword: String): Boolean
  def containsStopword(stopword: String): Boolean
  def containsStopwordStrict(stopword: String): Boolean = containsStopword(stopword)
}
