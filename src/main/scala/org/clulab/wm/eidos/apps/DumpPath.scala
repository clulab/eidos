package org.clulab.wm.eidos

import java.io.{File, FilenameFilter, PrintWriter}

import org.clulab.processors.{Document, Sentence}
import org.clulab.odin.impl.TokenPattern
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.utils.DependencyUtils
import org.clulab.utils.DependencyUtils._
import org.clulab.wm.eidos.utils.{Sourcer}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.ForkJoinPool

object DumpPath extends App {

  val proc = new FastNLPProcessor()

  def loadInstances(file: File): Unit = {
    //"m.0ccvx    m.05gf08    queens    belle_harbor    /location/location/contains    sen. charles e. schumer called on federal safety officials yesterday to reopen their investigation into the fatal crash of a passenger jet in belle_harbor , queens , because equipment failure , not pilot error , might have been the cause . ###END###"
    val filename = file.getAbsolutePath
    val pw = new PrintWriter(filename + ".nonlex_dep_path")

    var badSentence: Int = 0

    val source = scala.io.Source.fromFile(filename)
    // todo: make a buffered reader??? (for memory issues)
    val lines = source.getLines()
    //var lineCounter = 0
    while (lines.hasNext) {
      val line = lines.next()
//      if (lineCounter % 1000 == 0) {
//        println(s"Processing line ${lineCounter}")
//        println(s"CURRENT BAD SENTENCE: $badSentence")
//      }
//      lineCounter += 1
      //println(s"**LINE: $line")

      var fields = line.split("\t")
      //println(s"**FIELDS: ${fields.mkString(", ")}")

      var sentText = fields(5).split("###END###").head
      // Store the end punctuation
      val endPunct = sentText.trim.last
      //println(endPunct)

//      sentText = sentText.replaceAll("\\.", "") + endPunct
//      fields = fields.map(s => s.replaceAll("\\.", ""))
      var e1 = fields(2)//.split("_")
      var e2 = fields(3)//.split("_")

      val sentence: Sentence = mkPartialAnnotation(sentText).sentences.head
      //println(sentence.getSentenceText())
     // println(s"**WORDS: ${sentence.words.mkString(";")}")

      if (!sentence.words.contains(e1) || !sentence.words.contains(e2)) {
        badSentence += 1
        println(s"WARNING: bad sentence -- one of the entities not found! \n doc sentence text: ${sentence.getSentenceText()}")
        //println(s"CURRENT BAD SENTENCE: $badSentence")
      }  else {
        // Entity tokens
        val e1Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e1).unzip._2
        //println(s"\te1: $e1 $e1Int  ")//[${sentence.words(e1Int)}]")

        val e2Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e2).unzip._2
        //println(s"\te2: $e2 $e2Int ")// [${sentence.words(e2Int)}]")
        val pairs = for {
          a <- e1Ints
          b <- e2Ints
        } yield (a,b)
        val (e1Int, e2Int) = pairs.map(p => (p, math.abs(p._1 - p._2))).minBy(_._2)._1

        // Fully lexicalized
        //      writeFullyLexicalized(sentence, fields, e1Int, e2Int, e1, e2, pw)

        // Lexicalize only gove head
        writeLexicalizeOnlyGovHead(sentence, fields, e1Int, e2Int, e1, e2, pw)
      }

    }

    println(s"TOTAL BAD SENTENCE for file ${filename} = $badSentence")
    source.close()
    pw.close()
  }


  def mkPartialAnnotation(text: String): Document = {
//    val doc = proc.mkDocument(text)
    val doc = proc.mkDocumentFromTokens(Seq(text.split(" ")))
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
      //println(s"${sentence.getSentenceText()}")
      //println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
      //println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> lexPath ${path}")

      pw.println(toPrint)
    }
  }

  def writeLexicalizeOnlyGovHead(sentence: Sentence, fields: Seq[String],
               e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    // Shortest path from e1 to e1:
    // if you want to use this instead!
    val interval = if (e1Int < e2Int) Interval(e1Int, e2Int + 1) else Interval(e2Int, e1Int + 1)
    // Odin uses lastOption for getting mention.synHead, so that's what we're doing here
    //println(s"finding head")
    var govHead = DependencyUtils.findHeads(interval, sentence.dependencies.get)
      .lastOption
      .getOrElse(sentence.dependencies.get.roots.head)
    if (govHead == -1) {
      // If there is no head -- backoff to the shortest path between e1 and e2
      println("WARNING: no head found, using e1 as head!")
      govHead = e1Int
    }

    //println(s"finding shortest paths to head (${sentence.words(govHead)})")
    val shortestE1ToHead = sentence.dependencies.get.shortestPathEdges(e1Int, govHead, ignoreDirection = true)
    val shortestHeadToE2 = sentence.dependencies.get.shortestPathEdges(govHead, e2Int, ignoreDirection = true)
    //println(s"len shortestE1ToHead: ${shortestE1ToHead.length}")
    //println(s"len shortestHeadToE2: ${shortestHeadToE2.length}")

    //println(s"finding lexicalized paths to head (${sentence.words(govHead)})")
    val headLexicalizedPaths = for {
      i <- shortestE1ToHead
      j <- shortestHeadToE2
    } yield lexicalizedPathGovHead(sentence.words, govHead, i, j)
    //println("--> done.")

    for (path <- headLexicalizedPaths) {
      val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
      //println(s"${sentence.getSentenceText()}")
      //println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
      //println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> \n\t\theadlexPath: \t${path}")

      pw.println(toPrint)
    }

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
      //println(s"PATH E1 to HEAD: ${pathe1ToHead.mkString(";")}")
      val start = pathe1ToHead.head
      val leftWord = if (start._4 == ">") words(start._1) else words(start._2)
      //println(s"leftWord: $leftWord")
      val rightWord = if (start._4 == ">") words(start._2) else words(start._1)
      //println(s"rightWord: $rightWord")
      val startRel = start._4 + start._3
      out.append(s"$leftWord $startRel")

      val restOfPath = pathe1ToHead.slice(1, pathe1ToHead.length)
      if (restOfPath.nonEmpty) {
        //println("** rest of the path:")
        var lastWord = ""
        for (edge <- restOfPath) {
          val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
          //println(s"rightWord: $rightWord")
          val pathPiece = s"${edge._4}${edge._3}"
          //println(s"pathPiece: $pathPiece")
          out.append(pathPiece)
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
      //println(s"PATH HEAD to E2: ${pathHeadToE2.mkString(";")}")
      // I think that the head will be the first thing here, and the path will follow to e2, but check
      val starte2 = pathHeadToE2.head
      assert(starte2._1 == headInt || starte2._2 == headInt)
      //println(s"starte2: $starte2")

      var lastWord = ""
      for (edge <- pathHeadToE2) {
        val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
        out.append(s"${edge._4}${edge._3}")
        lastWord = rightWord
      }
      out.append(lastWord)
    } else {
      // nothing, we're done
    }


    out.mkString(" ")

  }


  def pathToText(path: Seq[(Int, Int, String, String)]): String = path.map(edge => edge._4 + edge._3).mkString(" ")

  def findFilesPrefix(collectionDir: String, prefix: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.startsWith(prefix)
    }

    val result = dir.listFiles(filter)
    if (result == null)
      throw Sourcer.newFileNotFoundException(collectionDir)
    result
  }

  //val fn = "/Users/bsharp/relationExtraction/RE/test.txt"
  val nCores = 20
  val dir = "/work/bsharp/relationExtraction/RE/chunked_test"
  val files = findFilesPrefix(dir, "x").par
  files.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nCores))
  for {
    file <- files
  } loadInstances(file)




}
