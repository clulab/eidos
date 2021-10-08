package org.clulab.wm.eidoscommon.utils

import java.io.PrintStream
import scala.annotation.tailrec

case class Edit(
  sourceString: String, targetString: String,
  prevSourceIndex: Int, prevTargetIndex: Int,
  nextSourceIndex: Int, nextTargetIndex: Int
) {

  // Do sourceCharOpt and targetCharOpt
  def print(printStream: PrintStream): Unit = printStream.println()

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
    extends Edit(sourceString, targetString, nextSourceIndex - 1, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream): Unit =
      Edit.printRow(
        printStream, "Confirmation",
        Some(prevSourceIndex), Some(getSourceChar),
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

class Insertion(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(sourceString, targetString, nextSourceIndex, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream): Unit =
      Edit.printRow(
        printStream, "Insertion",
        None, None,
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

// The source character has been misinterpreted as the target character.
class Substitution(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(sourceString, targetString, nextSourceIndex - 1, nextTargetIndex - 1, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream): Unit =
      Edit.printRow(
        printStream, "Substitution",
        Some(prevSourceIndex), Some(getSourceChar),
        Some(prevTargetIndex), Some(getTargetChar)
      )
}

class Deletion(sourceString: String, targetString: String, nextSourceIndex: Int, nextTargetIndex: Int)
    extends Edit(sourceString, targetString, nextSourceIndex - 1, nextTargetIndex, nextSourceIndex, nextTargetIndex) {

  override def print(printStream: PrintStream): Unit =
      Edit.printRow(
        printStream, "Deletion",
        Some(prevSourceIndex), Some(getSourceChar),
        None, None
      )
}

abstract class Editor(val editClass: Class[_], sourceString: String, targetString: String) {
  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int
  def getEdit(sourceIndex: Int, targetIndex: Int): Edit
}

class Confirmer(sourceString: String, targetString: String) extends Editor(classOf[Confirmation], sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar == targetChar) 0 else Integer.MAX_VALUE

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0 && sourceIndex == 0) 0
    else if (targetIndex == 0 || sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 1)(targetIndex - 1) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit =
      new Confirmation(sourceString, targetString, sourceIndex, targetIndex)
}

class Inserter(sourceString: String, targetString: String) extends Editor(classOf[Insertion], sourceString, targetString) {

  def getCost(targetChar: Char): Int = 1

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex)(targetIndex - 1) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit =
      new Insertion(sourceString, targetString, sourceIndex, targetIndex)

}

class Deleter(sourceString: String, targetString: String) extends Editor(classOf[Deletion], sourceString, targetString) {

  def getCost(sourceChar: Char): Int = 1

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(sourceString.charAt(sourceIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 1)(targetIndex) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit =
      new Deletion(sourceString, targetString, sourceIndex, targetIndex)
}

class Substituter(sourceString: String, targetString: String) extends Editor(classOf[Substitution], sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar != targetChar) 1 else Integer.MAX_VALUE

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0 && sourceIndex == 0) 0
    else if (targetIndex == 0 || sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 1)(targetIndex - 1) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit  =
      new Substitution(sourceString, targetString, sourceIndex, targetIndex)
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
  // These are recorded now in the order of preference for tie breaking where we want
  // deletions to win when the target text is shorter than the source.
  protected val editors: Array[Editor] = Array(
    new Deleter(sourceString, targetString),
    new Confirmer(sourceString, targetString),
    new Inserter(sourceString, targetString),
    new Substituter(sourceString, targetString)
  )
  protected val distances: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  // This keeps track of the index of the editor used at each position.
  protected val editorIndexes: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  protected val distance: Int = measure()
  protected lazy val edits: Array[Edit] = mkEdits()

  def getDistance: Int = distance

  protected def measure(): Int = {
    val costs = new Array[Int](editors.length)

    Range(0, sourceString.length + 1).foreach { sourceIndex =>
      Range(0, targetString.length + 1).foreach { targetIndex =>
        editors.zipWithIndex.foreach { case (editor, index) =>
          costs(index) = editor.calcCost(distances, sourceIndex, targetIndex)
        }

        val minCost = costs.min
        val editorIndex = costs.indexOf(minCost)

        distances(sourceIndex)(targetIndex) = minCost
        editorIndexes(sourceIndex)(targetIndex) = editorIndex
      }
    }
    distances(sourceString.length)(targetString.length)
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
        printStream.print(distances(sourceIndex)(targetIndex))
        printStream.print("\t")
      }
      printStream.println()
    }
  }

  protected def mkEdits(): Array[Edit] = {

    @tailrec
    def recMkEdits(edits: List[Edit], sourceIndex: Int, targetIndex: Int): List[Edit] = {
      if (sourceIndex == 0 && targetIndex == 0) edits
      else {
        val edit = editors(editorIndexes(sourceIndex)(targetIndex)).getEdit(sourceIndex, targetIndex)

        recMkEdits(edit :: edits, edit.prevSourceIndex, edit.prevTargetIndex)
      }
    }

    val edits = recMkEdits(Nil, sourceString.length, targetString.length)

    edits.toArray
  }
  
  def printEditsOn(printStream: PrintStream): Unit = {
    Edit.printHeader(printStream)
    edits.foreach(_.print(printStream))
  }

  def printSummaryOn(printStream: PrintStream): Unit = {
    val keys = editors
        .map(_.editClass.getName)
    val counts = edits
        .groupBy(_.getClass.getName)
        .mapValues(_.length)
    val headers = keys
        .map { key => key.substring(key.lastIndexOf('.') + 1) }
        .mkString("\t")
    val values = keys
        .map { key => counts.getOrElse(key, 0) }
        .mkString("\t")

    printStream.println(headers)
    printStream.println(values)
  }
}

object MED {

  def apply(sourceString: String, targetString: String): MED = new MED(sourceString, targetString)
}

object MEDApp extends App {
  val med = MED("Sunday", "Saturday")

  println(med.getDistance)
  med.printDistancesOn(System.out)
  med.printEditsOn(System.out)
  med.printSummaryOn(System.out)
}
