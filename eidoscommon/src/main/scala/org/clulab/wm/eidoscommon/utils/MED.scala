package org.clulab.wm.eidoscommon.utils

import java.io.PrintStream
import scala.annotation.tailrec

case class Edit(
  typ: Int,
  sourceString: String, targetString: String,
  prevSourceIndex: Int, prevTargetIndex: Int,
  nextSourceIndex: Int, nextTargetIndex: Int
) {

  def print(printStream: PrintStream, sourceString: String, targetString: String): Unit = printStream.println()

  // This Char may not exist for insertion.
  def getSourceChar: Char = sourceString.charAt(prevSourceIndex)

  // This Char may not exist for deletion.
  def getTargetChar: Char = targetString.charAt(prevTargetIndex)
}

object Edit {

  def printRow(printStream: PrintStream, col1: String, col2: String, col3: String, col4: String, col5: String): Unit =
      printStream.println(s"$col1\t$col2\t$col3\t$col4\t$col5")

  def intToString(intOpt: Option[Int]): String = intOpt.map(_.toString).getOrElse("-")

  def charToString(charOpt: Option[Char]): String = charOpt.map(Escaper.escape).getOrElse("_")

  def printRow(printStream: PrintStream, typ: String, sourceIndexOpt: Option[Int], sourceCharOpt: Option[Char],
      targetIndexOpt: Option[Int], targetCharOpt: Option[Char]): Unit = {
    printRow(
      printStream, typ,
      intToString(sourceIndexOpt), charToString(sourceCharOpt),
      intToString(targetIndexOpt), charToString(targetCharOpt)
    )
  }

  def printHeader(printStream: PrintStream): Unit =
      printRow(
        printStream, "Type",
        "Source Index", "Source Value",
        "Target Index", "Target Value"
      )
}

// The source character and target character match.
class Confirmation(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(MED.CONFIRMATION, sourceString, targetString, nextSourceIndex - 1, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream, sourceString: String, targetString: String): Unit =
      Edit.printRow(
        printStream, "Confirmation",
        Some(prevSourceIndex), Some(getSourceChar),
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

class Insertion(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(MED.INSERTION, sourceString, targetString, nextSourceIndex, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream, sourceString: String, targetString: String): Unit =
      Edit.printRow(
        printStream, "Insertion",
        None, None,
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

// The source character has been misinterpreted as the target character.
class Substitution(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(MED.SUBSTITUTION, sourceString, targetString, nextSourceIndex - 1, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream, sourceString: String, targetString: String): Unit =
      Edit.printRow(
        printStream, "Substitution",
        Some(prevSourceIndex), Some(getSourceChar),
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

class Deletion(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(MED.DELETION, sourceString, targetString, nextSourceIndex - 1, nextTargetIndex, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream, sourceString: String, targetString: String): Unit =
      Edit.printRow(
        printStream, "Deletion",
        Some(prevSourceIndex), Some(getSourceChar),
        None, None
      )
}

object Escaper {

  def escape(c: Char): String = c match {
    case '\r' => "\\r"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case ' ' => "\\s"
    case c => Character.toString(c)
  }
}

class MED(sourceString: String, targetString: String) {
  protected val distances: Array[Array[Int]] = Array.ofDim[Int](targetString.length + 1, sourceString.length + 1)
  // This keeps track of the type of edit needed at each position.
  protected val minIndexes: Array[Array[Int]] = Array.ofDim[Int](targetString.length + 1, sourceString.length + 1)
  protected val distance: Int = measure()
  protected lazy val edits: Array[Edit] = mkEdits()

  protected def getConfirmationCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar == targetChar) 0 else Integer.MAX_VALUE

  protected def getInsertionCost(c: Char): Int = 1
  
  protected def getDeletionCost(c: Char): Int = 1
  
  protected def getSubstitutionCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar != targetChar) 2 else Integer.MAX_VALUE

  protected def calcConfirmationCost(sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0 && sourceIndex == 0) 0
    else if (targetIndex == 0 || sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getConfirmationCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(targetIndex - 1)(sourceIndex - 1) + cost
    }
  }

  protected def calcInsertionCost(sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getInsertionCost(targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(targetIndex - 1)(sourceIndex) + cost
    }
  }
  
  protected def calcSubstitutionCost(sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0 && sourceIndex == 0) 0
    else if (targetIndex == 0 || sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getSubstitutionCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(targetIndex - 1)(sourceIndex - 1) + cost
    }
  }
  
  protected def calcDeletionCost(sourceIndex: Int, targetIndex: Int): Int = {
    if (sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getDeletionCost(sourceString.charAt(sourceIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(targetIndex)(sourceIndex - 1) + cost
    }
  }

  def getDistance: Int = distance

  protected def measure(): Int = {
    val costs = new Array[Int](4)

    Range(0, targetString.length + 1).foreach { targetIndex =>
      Range(0, sourceString.length + 1).foreach { sourceIndex =>
        costs(MED.CONFIRMATION) = calcConfirmationCost(sourceIndex, targetIndex)
        costs(MED.INSERTION) = calcInsertionCost(sourceIndex, targetIndex)
        costs(MED.SUBSTITUTION) = calcSubstitutionCost(sourceIndex, targetIndex)
        costs(MED.DELETION) = calcDeletionCost(sourceIndex, targetIndex)

        val minCost = costs.min
        val minIndex = costs.indexOf(minCost)

        distances(targetIndex)(sourceIndex) = minCost
        minIndexes(targetIndex)(sourceIndex) = minIndex
      }
    }
    distances(targetString.length)(sourceString.length)
  }
  
  def printDistancesOn(printStream: PrintStream): Unit = {
    printStream.print("\t")
    Range(0, sourceString.length + 1).foreach { sourceIndex =>
      if (sourceIndex > 0)
        printStream.print(sourceString.charAt(sourceIndex - 1))
      printStream.print("\t")
    }
    printStream.println()

    Range(0, targetString.length + 1).foreach { targetIndex =>
      Range(0, sourceString.length + 1).foreach { sourceIndex =>
        if (sourceIndex == 0) {
          if (targetIndex > 0)
            printStream.print(targetString.charAt(targetIndex - 1))
          printStream.print("\t")
        }
        printStream.print(distances(targetIndex)(sourceIndex))
        printStream.print("\t")
      }
      printStream.println()
    }
  }
  
  protected def getEdit(sourceIndex: Int, targetIndex: Int): Edit = {
    minIndexes(targetIndex)(sourceIndex) match {
      case MED.CONFIRMATION => new Confirmation(sourceString, targetString, sourceIndex, targetIndex)
      case MED.INSERTION => new Insertion(sourceString, targetString, sourceIndex, targetIndex)
      case MED.SUBSTITUTION => new Substitution(sourceString, targetString, sourceIndex, targetIndex)
      case MED.DELETION => new Deletion(sourceString, targetString, sourceIndex, targetIndex)
      case _ => throw new RuntimeException("Unknown edit type")
    }
  }
  
  protected def mkEdits(): Array[Edit] = {

    @tailrec
    def recMkEdits(edits: List[Edit], sourceIndex: Int, targetIndex: Int): List[Edit] = {
      if (sourceIndex == 0 && targetIndex == 0) edits
      else {
        val edit = getEdit(sourceIndex, targetIndex)

        recMkEdits(edit :: edits, edit.prevSourceIndex, edit.prevTargetIndex)
      }
    }

    val edits = recMkEdits(Nil, sourceString.length, targetString.length)

    edits.toArray
  }
  
  def printEditsOn(printStream: PrintStream, onlyErrors: Boolean): Unit = {
    Edit.printHeader(printStream)
    edits.foreach { edit =>
      if (!(onlyErrors && edit.typ == MED.CONFIRMATION))
        edit.print(printStream, sourceString, targetString)
    }
  }

  def printSummaryOn(printStream: PrintStream): Unit = {
    val counts = edits
        .groupBy(_.getClass.getName)
        .mapValues(_.length)
    val keys = counts.keys.toSeq.sorted
    val headers = keys
        .map { key => key.substring(key.lastIndexOf('.') + 1) }
        .mkString("\t")
    val values = keys
        .map(counts)
        .mkString("\t")

    printStream.println(headers)
    printStream.println(values)
  }
}

object MED {
  // These are recorded now in the order of preference for tie breaking where we want
  // deletions to win when the target text is shorter than the source.
  val DELETION = 0
  val CONFIRMATION = 1
  val INSERTION = 2
  val SUBSTITUTION = 3
}

object MEDApp extends App {
  val med = new MED("Sunday", "Saturday")

  println(med.getDistance)
  med.printDistancesOn(System.out)
  med.printEditsOn(System.out, onlyErrors = false)
  med.printSummaryOn(System.out)
}
