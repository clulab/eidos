package org.clulab.wm.eidos

import java.io.PrintWriter

import org.clulab.processors.{Document, Sentence}
import org.clulab.odin.impl.TokenPattern
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.utils.DependencyUtils
import org.clulab.utils.DependencyUtils._

import scala.collection.mutable.ArrayBuffer

object DumpPath extends App {

  val proc = new FastNLPProcessor()

  def mkPartialAnnotation(text: String): Document = {
    val doc = proc.mkDocument(text)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.parse(doc)
    doc.clear()
    doc
  }

  def writeFullyLexicalized(sentence: Sentence, fields: Seq[String],
               e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    // Shortest path from e1 to e1:
    // if you want to use this instead!
    val shortestE1E2: Seq[Seq[(Int, Int, String, String)]] = sentence.dependencies.get.shortestPathEdges(e1Int, e2Int, ignoreDirection = true)
    // todo: you can find the "highest" node in this path to serve as the trigger
    // todo: histogram of #hops between entities in diff datasets

    val fullyLexicalizedPaths = shortestE1E2.map(path => lexicalizedPath(sentence.words, path))
    for (path <- fullyLexicalizedPaths) {
      val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
      println(s"${sentence.getSentenceText()}")
      println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
      println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> lexPath ${path}")

      pw.println(toPrint)
    }
  }

  def writeLexicalizeOnlyGovHead(sentence: Sentence, fields: Seq[String],
               e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    // Shortest path from e1 to e1:
    // if you want to use this instead!
    val interval = if (e1Int < e2Int) Interval(e1Int, e2Int + 1) else Interval(e2Int, e1Int + 1)
    // Odin uses lastOption for getting mention.synHead, so that's what we're doing here
    val govHead = DependencyUtils.findHeads(interval, sentence.dependencies.get)
      .lastOption
      .getOrElse(sentence.dependencies.get.roots.head)
    val shortestE1ToHead = sentence.dependencies.get.shortestPathEdges(e1Int, govHead, ignoreDirection = true)
    val shortestHeadToE2 = sentence.dependencies.get.shortestPathEdges(govHead, e2Int, ignoreDirection = true)



    val headLexicalizedPaths = for {
      i <- shortestE1ToHead
      j <- shortestHeadToE2
    } yield lexicalizedPathGovHead(sentence.words, govHead, i, j)

    for (path <- headLexicalizedPaths) {
      val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
      println(s"${sentence.getSentenceText()}")
      println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
      println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> \n\t\theadlexPath: \t${path}")

      pw.println(toPrint)
    }
  }

  def loadInstances(filename: String): Unit = {
    //"m.0ccvx    m.05gf08    queens    belle_harbor    /location/location/contains    sen. charles e. schumer called on federal safety officials yesterday to reopen their investigation into the fatal crash of a passenger jet in belle_harbor , queens , because equipment failure , not pilot error , might have been the cause . ###END###"
    val pw = new PrintWriter(filename + ".nonlex_dep_path")

    val source = scala.io.Source.fromFile(filename)
    // todo: make a buffered reader??? (for memory issues)
    val lines = source.getLines().toSeq
    for ((line, lineIdx) <- lines.zipWithIndex) {
      if (lineIdx % 10 == 0) {
        println(s"Processing line ${lineIdx} of ${lines.length}")
      }
      val fields = line.split("\t")
      val e1 = fields(2)//.split("_")
      val e2 = fields(3)//.split("_")
      val sentText = fields(5).split("###END###").head
      val sentence: Sentence = mkPartialAnnotation(sentText).sentences.head
      //println(sentence.getSentenceText())

      // Entity tokens
      val e1Int: Int = sentence.words.indexOf(e1)
      //println(s"\te1: $e1 $e1Int  [${sentence.words(e1Int)}]")
      val e2Int = sentence.words.indexOf(e2)
      //println(s"\te2: $e2 $e2Int  [${sentence.words(e2Int)}]")

      // Fully lexicalized
//      writeFullyLexicalized(sentence, fields, e1Int, e2Int, e1, e2, pw)

      // Lexicalize only gove head
      writeLexicalizeOnlyGovHead(sentence, fields, e1Int, e2Int, e1, e2, pw)


    }

    source.close()
    pw.close()


  }


  // from, to, relation, direction
  def lexicalizedPath(words: Seq[String], path: Seq[(Int, Int, String, String)]): String = {
    val out = new ArrayBuffer[String]
    val start = path.head
    val leftWord = if (start._4 == ">") words(start._1) else words(start._2)
    val rightWord = if (start._4 == ">") words(start._2) else words(start._1)
    val startRel = start._4 + start._3
    out.append(s"$leftWord $startRel $rightWord")

    for (edge <- path.slice(1, path.length)) {
      val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
      out.append(s"${edge._4}${edge._3} $rightWord")
      }
    out.mkString(" ")
    }

  // from, to, relation, direction
  def lexicalizedPathGovHead(words: Seq[String],
                             headInt: Int,
                             pathe1ToHead: Seq[(Int, Int, String, String)],
                             pathHeadToE2: Seq[(Int, Int, String, String)]): String = {

    val out = new ArrayBuffer[String]

    // Case 1 -- both paths are empty
    if (pathe1ToHead.isEmpty && pathHeadToE2.isEmpty) {
      // shouldn't happen becuase the head should be only one or the other
      throw new RuntimeException("Both of the shortest paths are empty!")
    }

    // Check to make sure e1 isn't the head (i.e. there is a path)
    if (pathe1ToHead.nonEmpty) {
      val start = pathe1ToHead.head
      val leftWord = if (start._4 == ">") words(start._1) else words(start._2)
      val rightWord = if (start._4 == ">") words(start._2) else words(start._1)
      val startRel = start._4 + start._3
      out.append(s"$leftWord $startRel")

      val restOfPath = pathe1ToHead.slice(1, pathe1ToHead.length)
      if (restOfPath.nonEmpty) {
        var lastWord = ""
        for (edge <- restOfPath) {
          val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
          out.append(s"${edge._4}${edge._3}")
          //println(s"appended: ( ${edge._4}${edge._3} )")
          lastWord = rightWord
        }
        // I think we land on the governing head, but check
        //println(s"lastWord: $lastWord\tgovHead: ${words(headInt)}")
        assert(lastWord == words(headInt))
        out.append(words(headInt))
      } else {
        out.append(words(headInt))
      }
    } else {
      out.append(words(headInt))
    }

    // Check to make sure e2 isn't the head (i.e., there is a path)
    if (pathHeadToE2.nonEmpty) {
      // I think that the head will be the first thing here, and the path will follow to e2, but check
      val starte2 = pathHeadToE2.head
      assert(starte2._1 == headInt || starte2._2 == headInt)

      val restOfThePath2 = pathHeadToE2.slice(1, pathHeadToE2.length)
      if (restOfThePath2.nonEmpty) {
        var lastWord = ""
        for (edge <- restOfThePath2) {
          val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
          out.append(s"${edge._4}${edge._3}")
          lastWord = rightWord
        }
        out.append(lastWord)
      }
    } else {
      // nothing, we're done
    }


    out.mkString(" ")

  }


  def pathToText(path: Seq[(Int, Int, String, String)]): String = path.map(edge => edge._4 + edge._3).mkString(" ")

  val fn = "/Users/bsharp/relationExtraction/RE/train.txt"
  loadInstances(fn)




}
