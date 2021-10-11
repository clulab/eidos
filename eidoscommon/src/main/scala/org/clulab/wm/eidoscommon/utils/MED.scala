package org.clulab.wm.eidoscommon.utils

import java.io.PrintStream
import scala.annotation.tailrec

case class Edit(
  name: String,
  sourceString: String, targetString: String,
  prevSourceIndex: Int, prevTargetIndex: Int,
  nextSourceIndex: Int, nextTargetIndex: Int
) {

  def getSourceChars: String = sourceString.substring(prevSourceIndex, nextSourceIndex)

  def getTargetChars: String = targetString.substring(prevTargetIndex, nextTargetIndex)

  def print(printStream: PrintStream): Unit = {
    val sourceChars = getSourceChars
    val targetChars = getTargetChars

    Edit.printRow(printStream, name,
      if (sourceChars.nonEmpty) Some(prevSourceIndex) else None, getSourceChars,
      if (targetChars.nonEmpty) Some(prevTargetIndex) else None, getTargetChars
    )
  }
}

object Edit {

  def printRow(printStream: PrintStream, col1: String, col2: String, col3: String, col4: String, col5: String): Unit =
      printStream.println(s"$col1\t$col2\t$col3\t$col4\t$col5")

  def intToString(intOpt: Option[Int]): String = intOpt.map(_.toString).getOrElse("-")

  def charsToString(chars: String): String = if (chars.isEmpty) "_" else chars.flatMap(Escaper.escape)

  def printRow(printStream: PrintStream, typ: String, sourceIndexOpt: Option[Int], sourceChars: String,
      targetIndexOpt: Option[Int], targetChars: String): Unit = {
    printRow(
      printStream, typ,
      intToString(sourceIndexOpt), charsToString(sourceChars),
      intToString(targetIndexOpt), charsToString(targetChars)
    )
  }

  def printHeader(printStream: PrintStream): Unit =
      printRow(
        printStream, "Type",
        "Source Index", "Source Value",
        "Target Index", "Target Value"
      )
}

abstract class Editor(val name: String, sourceString: String, targetString: String) {
  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int
  def getEdit(sourceIndex: Int, targetIndex: Int): Edit

  def newEdit(prevSourceIndex: Int, prevTargetIndex: Int, nextSourceIndex: Int, nextTargetIndex: Int): Edit =
      new Edit(name, sourceString, targetString, prevSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)
}

// The source character and target character match.
class Confirmer(sourceString: String, targetString: String) extends Editor("Confirmation", sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar == targetChar) 0 else Integer.MAX_VALUE

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (sourceIndex == 0 && targetIndex == 0) 0
    else if (sourceIndex == 0 || targetIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 1)(targetIndex - 1) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit =
      newEdit(sourceIndex - 1, targetIndex - 1, sourceIndex, targetIndex)
}

class Inserter(sourceString: String, targetString: String) extends Editor("Insertion", sourceString, targetString) {

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
      newEdit(sourceIndex, targetIndex - 1, sourceIndex, targetIndex)
}

class Deleter(sourceString: String, targetString: String) extends Editor("Deletion", sourceString, targetString) {

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
      newEdit(sourceIndex - 1, targetIndex, sourceIndex, targetIndex)
}

// The source character has been misinterpreted as the target character.
class Substituter(sourceString: String, targetString: String) extends Editor("Substitution", sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int =
      if (sourceChar != targetChar) 2 else Integer.MAX_VALUE

  def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (sourceIndex == 0 && targetIndex == 0) 0
    else if (sourceIndex == 0 || targetIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1))

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 1)(targetIndex - 1) + cost
    }
  }

  def getEdit(sourceIndex: Int, targetIndex: Int): Edit  =
      newEdit(sourceIndex - 1, targetIndex - 1, sourceIndex, targetIndex)
}

class Transposer(sourceString: String, targetString: String) extends Editor("Transposition", sourceString, targetString) {

  def getCost(prevSourceChar: Char, prevTargetChar: Char, nextSourceChar: Char, nextTargetChar: Char): Int =
    if (prevSourceChar == nextTargetChar && nextSourceChar == prevTargetChar) 1 else Integer.MAX_VALUE

  override def calcCost(distances: Array[Array[Int]], sourceIndex: Int, targetIndex: Int): Int = {
    if (targetIndex == 0 && sourceIndex == 0) 0
    else if (targetIndex == 0 || sourceIndex == 0) Integer.MAX_VALUE
    else {
      val cost = getCost(
        sourceString.charAt(sourceIndex - 2), targetString.charAt(sourceIndex - 2),
        sourceString.charAt(sourceIndex - 1), targetString.charAt(targetIndex - 1)
      )

      if (cost == Integer.MAX_VALUE) cost
      else distances(sourceIndex - 2)(targetIndex - 2) + cost
    }
  }

  override def getEdit(sourceIndex: Int, targetIndex: Int): Edit =
      newEdit(sourceIndex - 2, targetIndex - 2, sourceIndex, targetIndex)
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

class MED(sourceString: String, targetString: String, allowSubstitute: Boolean = true, allowTranspose: Boolean = false) {
  // These are recorded now in the order of preference for tie breaking where we want
  // deletions to win when the target text is shorter than the source.
  protected val editors: Array[Editor] =
      Array(
        new Deleter(sourceString, targetString),
        new Confirmer(sourceString, targetString),
        new Inserter(sourceString, targetString)
      ) ++ {
        if (allowSubstitute) Array(new Substituter(sourceString, targetString))
        else Array.empty[Editor]
      } ++ {
        if (allowTranspose) Array(new Transposer(sourceString, targetString))
        else Array.empty[Editor]
      }

  protected val distances: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  // This keeps track of the index of the editor used at each position.
  protected val editorIndexes: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  protected val distance: Int = measure()
  protected lazy val edits: Array[Edit] = mkEdits()

  def getDistance: Int = distance

  protected def measure(): Int = {
    val costs = new Array[Int](editors.length)
    val sourceRange = Range(0, sourceString.length + 1)
    val targetRange = Range(0, targetString.length + 1)

    sourceRange.foreach { sourceIndex =>
      targetRange.foreach { targetIndex =>
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
    val sourceRange = Range(0, sourceString.length + 1)
    val targetRange = Range(0, targetString.length + 1)

    printStream.print("\t")
    sourceRange.foreach { sourceIndex =>
      if (sourceIndex > 0)
        printStream.print(sourceString.charAt(sourceIndex - 1))
      printStream.print("\t")
    }
    printStream.println()

    targetRange.foreach { targetIndex =>
      sourceRange.foreach { sourceIndex =>
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
        .map(_.name)
    val counts = edits
        .groupBy(_.name)
        .mapValues(_.length)
    val headers = keys
        .mkString("\t")
    val values = keys
        .map { key => counts.getOrElse(key, 0) }
        .mkString("\t")

    printStream.println(headers)
    printStream.println(values)
  }
}

object MED {

  def apply(sourceString: String, targetString: String, allowSubstitute: Boolean = true, allowTranspose: Boolean = false): MED =
      new MED(sourceString, targetString, allowSubstitute, allowTranspose)
}

object MEDApp extends App {
  val med = MED("Sunday", "Saturday")

  println(med.getDistance)
  med.printDistancesOn(System.out)
  med.printEditsOn(System.out)
  med.printSummaryOn(System.out)
}
