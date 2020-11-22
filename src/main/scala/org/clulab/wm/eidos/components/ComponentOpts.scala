package org.clulab.wm.eidos.components

// Set all true by default.
case class ComponentOpts(
  // proc
  proc: Boolean = true,
  // finder
  finders: Boolean = true,
  // odin
  attachmentHandler: Boolean = true,
  conceptExpander: Boolean = true,
  corefHandler: Boolean = true,
  hedgingHandler: Boolean = true,
  mostCompleteEventsKeeper: Boolean = true,
  negationHandler: Boolean = true,
  nestedArgumentExpander: Boolean = true,
  stopwordManager: Boolean = true,
  // eidos
  adjectiveGrounder: Boolean = true,
  eidosSentenceClassifier: Boolean = true,
  ontologyHandler: Boolean = true
) {

  def withoutGrounding = copy(ontologyHandler = false)
}

object ComponentOpts {
  protected lazy val cachedAll = new ComponentOpts()
  protected lazy val cachedNone = new ComponentOpts(
    false, false, false, false, false,
    false, false, false, false, false,
    false, false, false
  )

  def apply(): ComponentOpts = all()

  def all(): ComponentOpts = cachedAll

  def none(): ComponentOpts = cachedNone

  def proc(): ComponentOpts = cachedNone.copy(proc = true)

  def finders(): ComponentOpts = cachedNone.copy(proc = true, finders = true)

  def odin(): ComponentOpts = cachedAll.copy(adjectiveGrounder = false, eidosSentenceClassifier = false, ontologyHandler = false)

  def eidos(): ComponentOpts = cachedAll

  def eidosWithoutGrounding(): ComponentOpts = cachedAll.copy(ontologyHandler = false)
}
