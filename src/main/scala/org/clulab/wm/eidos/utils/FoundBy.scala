package org.clulab.wm.eidos.utils

import org.clulab.odin.Mention

class FoundBy(protected val foundBy: String) {

  def add(mention: Mention): String = add(mention.foundBy)

  def add(rule: String): String = {
    require(!rule.contains(FoundBy.concatter))
    require(!(foundBy.last == FoundBy.concatter.head && rule.head == FoundBy.concatter.last))

    FoundBy.concatter + rule
  }

  def get: String = foundBy
}

object FoundBy {
  val splitter = "\\+\\+"
  val concatter = "++"

  assert(concatter.size == 2)

  def apply(mention: Mention): FoundBy = new FoundBy(mention.foundBy)

  def apply(foundBy: String): FoundBy  = new FoundBy(foundBy)

  def split(foundBy: String): Array[String] = foundBy.split(splitter)

  def concat(mentions: Seq[Mention]): String = {
    require(mentions.nonEmpty)

    val foundBy = mentions
        .map(_.foundBy)
        .flatMap(split)
        .distinct
        .mkString(concatter)

    foundBy
  }
}