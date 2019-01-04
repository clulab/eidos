package org.clulab.wm.eidos.utils

trait Namer {
  def name: String

  override def toString: String = name
}

class PassThruNamer(val name: String) extends Namer