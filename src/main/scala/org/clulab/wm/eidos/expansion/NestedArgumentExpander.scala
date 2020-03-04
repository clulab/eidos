package org.clulab.wm.eidos.expansion

import org.clulab.odin.Mention

import scala.annotation.tailrec

class NestedArgumentExpander {

  def traverse(odinMentions: Seq[Mention]): Seq[Mention] = {
    // Dig in and get any Mentions that currently exist only as arguments, so that they get to be part of the state.
    @tailrec
    def traverse(ms: Seq[Mention], results: Seq[Mention], seen: Set[Mention]): Seq[Mention] = {
      ms match {
        case Nil => results
        case m +: rest if !seen.contains(m) =>
          //DisplayUtils.shortDisplay(m)
          val args = m.arguments.values.flatten
          traverse(rest ++ args, m +: results, seen + m)
        case m +: rest => traverse(rest, results, seen)
      }
    }

    traverse(odinMentions, Seq.empty, Set.empty)
  }
}
