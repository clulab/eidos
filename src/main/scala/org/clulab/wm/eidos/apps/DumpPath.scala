package org.clulab.wm.eidos

import java.io.{File, FilenameFilter, PrintWriter}

import org.clulab.processors.{Document, Sentence}
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.utils.DependencyUtils
import org.clulab.utils.DependencyUtils._
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.matching.Regex

object DumpPath extends App {

  val proc = new FastNLPProcessor()

  def findClosestOccurrences(sentence: Sentence, e1: String, e2: String): (Int, Int) = {
  // Entity tokens
  val e1Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e1).unzip._2
  //println(s"\te1: $e1 $e1Int  ")//[${sentence.words(e1Int)}]")

  val e2Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e2).unzip._2
  //println(s"\te2: $e2 $e2Int ")// [${sentence.words(e2Int)}]")
  val pairs = for {
    a <- e1Ints
    b <- e2Ints
  } yield (a,b)

  pairs
    // Don't allow to be the same index
    .filterNot(p => p._1 == p._2)
    // Zip with the distance
    .map(p => (p, math.abs(p._1 - p._2)))
    // Find the shortest distance (recall from above, won't be 0)
    .minBy(_._2)._1
}



  def repairBrokenEntities(sentenceText: String, entity: String): String = {
    if (sentenceText.contains(entity)) {
      return sentenceText
    } else {
      // find the broken entity
      val entityNoUnderScore = entity.replaceAll("_", " ")
      val sentTextNoUnderscore = sentenceText.replaceAll("_", " ")
      //println(s"entityNoUnderScore: [[$entityNoUnderScore]]")
      if (!sentTextNoUnderscore.contains(entityNoUnderScore)) {
        //println(s"WARNING: sentence ${sentTextNoUnderscore} does not contain entity ${entity}, even with no underscores!")
        //m.04c27w1	m.06_7wjm	mountain_road	music_mountain	NA	music_mountain , 225 music_mountain_road . ###END###
        return sentenceText
      } else {
        // find the interval of the entity
        val start = sentTextNoUnderscore.indexOf(entityNoUnderScore)
        val end = start + entityNoUnderScore.length
        val doesItWork = sentenceText.slice(start, end)
        //println (s"doesItWork: [[$doesItWork]]")
        val newSent = sentenceText.slice(0, start) + doesItWork.replaceAll(" ", "_") + sentenceText.slice(end, sentenceText.length)
        //println (s"newSent: [[$newSent]]")
        return newSent
      }
    }
  }

  def loadInstances(file: File,
                    outputSuffix: String,
                    writeMethod: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit): Unit = {
    //"m.0ccvx    m.05gf08    queens    belle_harbor    /location/location/contains    sen. charles e. schumer called on federal safety officials yesterday to reopen their investigation into the fatal crash of a passenger jet in belle_harbor , queens , because equipment failure , not pilot error , might have been the cause . ###END###"
    val filename = file.getAbsolutePath
    val pw = new PrintWriter(filename + outputSuffix)

    var badSentence: Int = 0

    val source = scala.io.Source.fromFile(filename)
    // todo: make a buffered reader??? (for memory issues)
    val lines = source.getLines()
    //var lineCounter = 0
    while (lines.hasNext) {
      val line = lines.next()
      //println(s"**LINE: $line")

      val fields = line.split("\t")
      println(s"**FIELDS (len=${fields.length}): ${fields.mkString(", ")}")

      var sentText = fields(5).split("###END###").head
      val e1 = fields(2).toLowerCase//.split("_")
      val e2 = fields(3).toLowerCase//.split("_")
      sentText = repairBrokenEntities(sentText, e1)
      sentText = repairBrokenEntities(sentText, e2)


      val sentence: Sentence = mkPartialAnnotation(sentText).sentences.head
      //println(sentence.getSentenceText())
      //println(s"**WORDS: ${sentence.words.mkString(";")}")

      if (!sentence.words.contains(e1) || !sentence.words.contains(e2)) {
        println("SENTENCE WORDS:")
        sentence.words.foreach(w => println(s"  [$w]"))
        println(s"E1: [$e1]\tE2: [$e2]")
        val longestEntity = Seq(e1, e2).maxBy(_.length)
        val shortestEntity = Seq(e1, e2).minBy(_.length)
        // Substring situation AND the longer entity appears more than once
        if (longestEntity.contains(shortestEntity) && (sentence.words.indexOf(longestEntity) != sentence.words.lastIndexOf(longestEntity))) {
          val (e1Int, e2Int) = findClosestOccurrences(sentence, longestEntity, longestEntity)
          writeMethod(sentence, fields, e1Int, e2Int, e1, e2, pw)
        } else {
          badSentence += 1
          println(s"WARNING: bad sentence -- one of the entities not found! \n doc sentence text: ${sentence.getSentenceText()}")
          //println(s"CURRENT BAD SENTENCE: $badSentence")
          val toPrint = (fields.slice(0, 5) ++ Seq(longestEntity)).mkString("\t")
          pw.println(toPrint)
        }

      }  else {
        // Default, normal case!
        val (e1Int, e2Int) = findClosestOccurrences(sentence, e1, e2)
        writeMethod(sentence, fields, e1Int, e2Int, e1, e2, pw)
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

    // Print only one!! The first will do!
    val path = fullyLexicalizedPaths.head
    val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
    pw.println(toPrint)
    //println(s"${sentence.getSentenceText()}")
    //println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
    //println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> lexPath ${path}")
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

    // Print only one!! The first will do!
    val path = headLexicalizedPaths.head
    val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
    pw.println(toPrint)
    //println(s"${sentence.getSentenceText()}")
    //println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
    //println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> \n\t\theadlexPath: \t${path}")



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

  def findFilesSuffix(collectionDir: String, suffix: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(suffix)
    }

    val result = dir.listFiles(filter)
    if (result == null)
      throw Sourcer.newFileNotFoundException(collectionDir)
    result
  }

  def countUnique(file:File): Unit = {
    val unique = scala.collection.mutable.Set[String]()
    val source = scala.io.Source.fromFile(file)
    // todo: make a buffered reader??? (for memory issues)
    val lines = source.getLines()
    //var lineCounter = 0
    while (lines.hasNext) {
      val line = lines.next()
      unique.add(line)
    }
    println(s"There are ${unique.size} unique lines!")
  }

  //val fn = "/Users/bsharp/relationExtraction/RE/test.txt"
  val nCores = 20
  val dir = "/work/bsharp/relationExtraction/RE/chunked_train"

  //  val outputSuffix = ".deps.headLex"
  //  val mode: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit = writeLexicalizeOnlyGovHead

  val outputSuffix = ".deps.fullyLex"
  val mode: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit = writeFullyLexicalized



  val files = findFilesPrefix(dir, "x").par
//  val outputSuffix = ".deps.headLex"
//  val dir = "/Users/bsharp/relationExtraction/gids/g"
//  val files = findFilesSuffix(dir, ".txt").par
//  val files = findFilesPrefix(dir, ".x").par
  files.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nCores))
  for {
    file <- files
    } loadInstances(file, outputSuffix, mode)

}



object DumpPathGids extends App {

//  val proc = new FastNLPProcessor()
  lazy val procClu = new CluProcessor()
  lazy val procFast = new FastNLPProcessor()
  val ENTITY_1 = "ENTITY1"
  val ENTITY_2 = "ENTITY2"


  def findClosest(A: Seq[Int], B: Seq[Int]): (Int, Int) = {
    val pairs = for {
      a <- A
      b <- B
    } yield (a,b)

    //println(s"pairs: ${pairs.mkString(";")}")
    pairs
      // Don't allow to be the same index
      .filterNot(p => p._1 == p._2)
      // Zip with the distance
      .map(p => (p, math.abs(p._1 - p._2)))
      // Find the shortest distance (recall from above, won't be 0)
      .minBy(_._2)._1

  }

  def findClosestOccurrences(sentence: Sentence, e1: String, e2: String): (Int, Int) = {
    // Entity tokens
    val e1Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e1).unzip._2
    val e2Ints: Seq[Int] = sentence.words.zipWithIndex.filter(_._1 == e2).unzip._2

    findClosest(e1Ints, e2Ints)
  }

  def repairBrokenEntities1(textIn: String, entityIn: String): (String, String) = {
    //println(s"Checking entity: $entityIn\t Sentence: ${textIn}")
    var text: String = textIn
    var entity: String = entityIn

    if (entity.contains("[") || entity.contains("]") || entity.contains("(") || entity.contains(")") || entity.contains("{") || entity.contains("}")) {
      println(s"!!! REMOVED PARENS IN: ${entityIn}")
      val repairedEnt = entity.replaceAll("(\\[|\\]|\\(|\\)|\\{|\\})", "")
      val origEntityAsRegex = entity.replaceAll("(\\[|\\]|\\(|\\)|\\{|\\})", ".")
      text = text.replaceAll(origEntityAsRegex, repairedEnt)
      entity = repairedEnt
    }

    (text, entity)
  }


  def repairBrokenEntities(textIn: String, entityIn: String): (String, String) = {
    //println(s"Checking entity: $entityIn\t Sentence: ${textIn}")
    var text: String = textIn
    var entity: String = entityIn

    if (entity.contains("[") || entity.contains("]") || entity.contains("(") || entity.contains(")") || entity.contains("{") || entity.contains("}")) {
      val repairedEnt = entity.replaceAll("(\\[|\\]|\\(|\\)|\\{|\\})", "")
      val origEntityAsRegex = entity.replaceAll("(\\[|\\]|\\(|\\)|\\{|\\})", ".")
      text = text.replaceAll(origEntityAsRegex, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains("_")) {
      val repairedEnt = entity.replaceAll("_", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains(" ")) {
      val repairedEnt = entity.replaceAll(" ", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains(""""""")) {
      val repairedEnt = entity.replaceAll(""""""", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains(",")) {
      val repairedEnt = entity.replaceAll(",", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains("&")) {
      val repairedEnt = entity.replaceAll("&", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains("'")) {
      val repairedEnt = entity.replaceAll("'", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains(".")) {
      val repairedEnt = entity.replaceAll("\\.", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.contains("/")) {
      val repairedEnt = entity.replaceAll("/", "")
      text = text.replaceAll(entity, repairedEnt)
      entity = repairedEnt
    }

    if (entity.endsWith("_")) {
      val entityTrimmed = entity.stripSuffix("_")
      text = text.replaceAll(entity, entityTrimmed)
      entity = entityTrimmed
    }

    if (entity.endsWith(".")) {
      val entityTrimmed = entity.stripSuffix(".")
      text = text.replaceAll(entity, entityTrimmed)
      entity = entityTrimmed
    }

    if (text.contains(entity)) {
      return (text, entity)

    } else {
      // find the broken entity
      val entityNoUnderScore = entity.replaceAll("_", " ")
      val sentTextNoUnderscore = text.replaceAll("_", " ")
      //println(s"entityNoUnderScore: [[$entityNoUnderScore]]")
      if (!sentTextNoUnderscore.contains(entityNoUnderScore)) {
        //println(s"WARNING: sentence ${sentTextNoUnderscore} does not contain entity ${entity}, even with no underscores!")
        //m.04c27w1	m.06_7wjm	mountain_road	music_mountain	NA	music_mountain , 225 music_mountain_road . ###END###
        return (text, entity)
      } else {
        // find the interval of the entity
        val start = sentTextNoUnderscore.indexOf(entityNoUnderScore)
        val end = start + entityNoUnderScore.length
        val doesItWork = text.slice(start, end)
        //println (s"doesItWork: [[$doesItWork]]")
        val repaired = text.slice(0, start) + doesItWork.replaceAll(" ", "_") + text.slice(end, text.length)
        //println (s"repaired: [[$repaired]]")
        return (repaired, entity)
      }
    }
  }

  def replaceEntities(text: String, entity: String, replacement: String): String = {
   // println(s"looking for ${entity} in ${text}")
    val regex = s"(?<![a-zA-Z0-9_])${entity}(?![a-zA-Z0-9_])".r
    if (text.contains(entity)) {
      //val x = "dksd".r
      //x.repl
      val replaced = regex.replaceAllIn(text, replacement)
      //println(s"replaced: ${replaced}\n")
      return replaced

    } else {
      // find the broken entity
      val entityNoUnderScore = entity.replaceAll("_", " ")
      val sentTextNoUnderscore = text.replaceAll("_", " ")
      //println(s"entityNoUnderScore: [[$entityNoUnderScore]]")
      if (!sentTextNoUnderscore.contains(entityNoUnderScore)) {
        //println(s"WARNING: sentence ${sentTextNoUnderscore} does not contain entity ${entity}, even with no underscores!\n")
        //m.04c27w1	m.06_7wjm	mountain_road	music_mountain	NA	music_mountain , 225 music_mountain_road . ###END###
        return text
      } else {
        // find the interval of the entity
        val start = sentTextNoUnderscore.indexOf(entityNoUnderScore)
        val end = start + entityNoUnderScore.length
        val doesItWork = text.slice(start, end)
        //println (s"doesItWork: [[$doesItWork]]")
        val repaired = text.slice(0, start) + doesItWork.replaceAll(" ", "_") + text.slice(end, text.length)
        //println (s"repaired after underscore fix: [[$repaired]]\n")
        return regex.replaceAllIn(repaired, replacement)
      }
    }
  }

  // Make the version of the entity
  def finalEntity(e: String): String = {
    procFast.mkDocument(e.replaceAll("_", " "))
      .sentences
      .flatMap(_.words)
      .mkString(" ")
      .replaceAll("-LSB-", "[")
      .replaceAll("-RSB-", "]")
      .replaceAll("-LRB-", "(")
      .replaceAll("-RRB-", ")")
  }

  def loadInstances(file: File,
                    outputSuffix: String,
                    writeMethod: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit
                    ): Unit = {
    //"m.0ccvx    m.05gf08    queens    belle_harbor    /location/location/contains    sen. charles e. schumer called on federal safety officials yesterday to reopen their investigation into the fatal crash of a passenger jet in belle_harbor , queens , because equipment failure , not pilot error , might have been the cause . ###END###"
    val filename = file.getAbsolutePath
    val pw = new PrintWriter(filename + outputSuffix)

    var badSentence: Int = 0
    var aggregation: Int = 0

    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines()
    while (lines.hasNext) {
      val line = lines.next()
      //println(s"\n-------------------------------------------------------------\n**LINE: $line")

      val fields = line.split("\t")

      //println(s"**FIELDS (len=${fields.length}): ${fields.mkString(", ")}")

      val documentTextOrig = fields(5).toLowerCase.split("(###end###)|(###END###)").head
      val e1Orig = fields(2).toLowerCase//.split("_")
      val e2Orig = fields(3).toLowerCase//.split("_")

      var e1: String = ""
      var e2: String = ""
      var documentText: String = ""
      if (e1Orig.length >= e2Orig.length) {
        val repaired1 = repairBrokenEntities1(documentTextOrig, e1Orig)
        e1 = repaired1._2
        val repaired2 = repairBrokenEntities1(repaired1._1, e2Orig)
        e2 = repaired2._2

        val docText1 = replaceEntities(repaired2._1, e1, ENTITY_1)
        documentText = replaceEntities(docText1, e2, ENTITY_2)

      } else {
        val repaired1 = repairBrokenEntities1(documentTextOrig, e2Orig)
        e2 = repaired1._2
        val repaired2 = repairBrokenEntities1(repaired1._1, e1Orig)
        e1 = repaired2._2

        val docText1 = replaceEntities(repaired2._1, e2, ENTITY_2)
        documentText = replaceEntities(docText1, e1, ENTITY_1)
      }

      val finalFields = fields.take(2) ++ Seq(finalEntity(e1Orig), finalEntity(e2Orig)) ++ fields.takeRight(2) //2
      //println("FINALFIELDS:" + finalFields.mkString(", "))

      // replace entities with sanitized versions.  This should help parsing too!
      //val docText1 = replaceEntities(docRepaired, e1, ENTITY_1)
      //val documentText = replaceEntities(docText1, e2, ENTITY_2)

      val document: Document = mkPartialAnnotation(documentText)
      val hasBoth = document.sentences.filter(s => (s.words.contains(ENTITY_1) && s.words.contains(ENTITY_2)))
      if (hasBoth.isEmpty) {
        // There is no single sentence that contains both entities:
        if(!document.sentences.flatMap(_.words).contains(ENTITY_1) || !document.sentences.flatMap(_.words).contains(ENTITY_2)) {
          // If the doc not contain both anywhere at all!
          println(s"WARNING: Bad instance \n \t $line")
          document.sentences.foreach(s => println(s"\t  * ${s.words.map(w => s"[$w]").mkString(";")}"))
          badSentence += 1
          // Backoff -- just use the two entities
          val toPrint = (finalFields.take(5) ++ Seq(s"$e1 $e2")).mkString("\t")
          pw.println(toPrint)

        } else {
          // AGGREGATION: Both entities are somewhere -- find the closest sentences with each and join with semi-colon, reparse
          val sentencesWithE1: Array[Int] = document.sentences.zipWithIndex.filter(_._1.words.contains(ENTITY_1)).unzip._2
          val sentencesWithE2: Array[Int] = document.sentences.zipWithIndex.filter(_._1.words.contains(ENTITY_2)).unzip._2
          if (sentencesWithE1.isEmpty || sentencesWithE2.isEmpty) {
            println("I thought that both entities were contained....?")
            document.sentences.foreach(s => println(s"\t  * ${s.words.map(w => s"[$w]").mkString(";")}"))
            throw new RuntimeException("One of the entities not found, but it should have been...")
          }
          // Pick which sentences to use
          val (sentE1, sentE2) = findClosest(sentencesWithE1, sentencesWithE2)
          val sentence1 = document.sentences(sentE1).words.dropRight(1)
          val sentence2 = document.sentences(sentE2).words
          val aggDoc = mkAggregationAnnotation(sentence1, sentence2)
          val hasBoth2 = aggDoc.sentences.filter(s => (s.words.contains(ENTITY_1) && s.words.contains(ENTITY_2)))
          if (hasBoth2.isEmpty) {
            println(s"WARNING: Bad aggregation instance \n \t $line")
            document.sentences.foreach(s => println(s"\t  * ${s.words.map(w => s"[$w]").mkString(";")}"))
            badSentence += 1
            // Backoff -- just use the two entities
            val toPrint = (finalFields.take(5) ++ Seq(s"$e1 $e2")).mkString("\t")
            pw.println(toPrint)
          } else {
            val shortestSentence = hasBoth2.minBy(_.words.length)
            val (e1Int, e2Int) = findClosestOccurrences(shortestSentence, ENTITY_1, ENTITY_2)
            writeMethod(shortestSentence, finalFields, e1Int, e2Int, ENTITY_1, ENTITY_2, pw)
            aggregation += 1
          }



//          // Get the roots to use
//          val root1Int = sentence1.dependencies.get.roots.head
//          val root2Int = sentence2.dependencies.get.roots.head
//          // Pick the occurrences of the entities to use
//          val e1Int = sentence1.words.zipWithIndex.filter(_._1 == e1).minBy(elem => math.abs(elem._2 - root1Int))._2
//          val e2Int = sentence2.words.zipWithIndex.filter(_._1 == e2).minBy(elem => math.abs(elem._2 - root2Int))._2
//
//          writeMethodBackup(sentence1, sentence2, fields, e1Int, e2Int, e1, e2, pw)
//          aggregation += 1

        }

      } else {
        // Good! We have sentences that contain both entities!!
        // Pick the shortest?
        val shortestSentence = hasBoth.minBy(_.words.length)
        val (e1Int, e2Int) = findClosestOccurrences(shortestSentence, ENTITY_1, ENTITY_2)
        writeMethod(shortestSentence, finalFields, e1Int, e2Int, ENTITY_1, ENTITY_2, pw)

      }
    }


    println(s"TOTAL BAD SENTENCE for file ${filename} = $badSentence")
    println(s"Total aggregation for this file ${filename} = $aggregation")
    source.close()
    pw.close()
  }


  def mkPartialAnnotation(text: String): Document = {
    val doc = procFast.mkDocument(text)
    procFast.tagPartsOfSpeech(doc)
    procFast.lemmatize(doc)
    procFast.parse(doc)
    doc.clear()

    doc
  }

  def mkAggregationAnnotation(s1: Seq[String], s2: Seq[String]):Document = {
    val doc = procFast.mkDocumentFromTokens(Seq((s1 ++ Seq(";")) ++ s2))
    procFast.tagPartsOfSpeech(doc)
    procFast.lemmatize(doc)
    procFast.parse(doc)
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


    val path = fullyLexicalizedPaths
      // Print only one!! The first will do!
      .head
      // put the entities back to the desired final format
      .replaceAll(ENTITY_1, "@entity").replaceAll(ENTITY_2, "@entity")

    val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
    pw.println(toPrint)

  }

  def writeFullyLexicalizedWithPOS(sentence: Sentence, fields: Seq[String],
                            e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    // Shortest path from e1 to e1:
    // if you want to use this instead!
    val shortestE1E2: Seq[Seq[(Int, Int, String, String)]] = sentence.dependencies.get.shortestPathEdges(e1Int, e2Int, ignoreDirection = true)
    // todo: you can find the "highest" node in this path to serve as the trigger
    // todo: histogram of #hops between entities in diff datasets

    val fullyLexicalizedPaths = shortestE1E2.map(path => lexicalizedPathWithPOS(sentence.words, sentence.tags.get, path))

    val path = fullyLexicalizedPaths
      // Print only one!! The first will do!
      .head
      // put the entities back to the desired final format
      .replaceAll(ENTITY_1, "@entity").replaceAll(ENTITY_2, "@entity")

    val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
    pw.println(toPrint)

  }

  def writeLexicalizeOnlyGovHead(sentence: Sentence, fields: Seq[String],
                                 e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    // Shortest path from e1 to e1:
    val interval = if (e1Int < e2Int) Interval(e1Int, e2Int + 1) else Interval(e2Int, e1Int + 1)
    // Odin uses lastOption for getting mention.synHead, so that's what we're doing here
    var govHead = DependencyUtils.findHeads(interval, sentence.dependencies.get)
      .lastOption
      .getOrElse(sentence.dependencies.get.roots.head)
    if (govHead == -1) {
      // If there is no head -- backoff to the shortest path between e1 and e2
      println("WARNING: no head found, using e1 as head!")
      govHead = e1Int
    }

    val shortestE1ToHead = sentence.dependencies.get.shortestPathEdges(e1Int, govHead, ignoreDirection = true)
    val shortestHeadToE2 = sentence.dependencies.get.shortestPathEdges(govHead, e2Int, ignoreDirection = true)

    val headLexicalizedPaths = for {
      i <- shortestE1ToHead
      j <- shortestHeadToE2
      if i.nonEmpty || j.nonEmpty
    } yield lexicalizedPathGovHead(sentence.words, govHead, i, j)

    // Print only one!! The first will do!
    val path = if (headLexicalizedPaths.nonEmpty) headLexicalizedPaths.head else {
      println("**!!** WARNING: couldn't find dep path, but thought I would be able to!")
      s"$e1 $e2"
    }
    val finalPath = path.replaceAll(ENTITY_1, "@entity").replaceAll(ENTITY_2, "@entity")

    val toPrint = (fields.slice(0, 5) ++ Seq(finalPath)).mkString("\t")
    pw.println(toPrint)

  }

  // AGGREGATION VERSION
  def writeLexicalizeOnlyGovHead(sentenceE1: Sentence, sentenceE2: Sentence, fields: Seq[String],
                                 e1Int: Int, e2Int: Int, e1: String, e2: String, pw: PrintWriter): Unit = {
    //println("Aggregating sentences...")
    // sentence 1 Root:
    var roots1 = sentenceE1.dependencies.get.roots
    //println(s"sentenceE1 roots:")
    //for (r <- sentenceE1.dependencies.get.roots) {
    //  println(s"\t${r} = ${sentenceE1.words(r)}")
    //}
    if (roots1.isEmpty) {
      // If there is no head -- backoff to the shortest path between e1 and e2
      println("WARNING: no root found, using e1 as head!")
      roots1 = Set(e1Int)
    }

    // sentence 2 Root:
    var roots2 = sentenceE2.dependencies.get.roots
    //println(s"sentenceE2 roots:")
//    for (r <- sentenceE2.dependencies.get.roots) {
//      println(s"\t${r} = ${sentenceE2.words(r)}")
//    }
    if (roots2.isEmpty) {
      // If there is no head -- backoff to the shortest path between e1 and e2
      println("WARNING: no root found, using e2 as head!")
      roots2 = Set(e2Int)
    }
//    println(s"sentence1: ${sentenceE1.getSentenceText()}")
//    println(s"sentence2: ${sentenceE2.getSentenceText()}")
//    println(s"E1: e1Int=$e1Int\troot1=${roots1.mkString(";")}")
//    println(s"E2: e2Int=$e2Int\troot2=${roots2.mkString(";")}")


    //println("E1 PATH HUNTING:")
    val pathsForRootsToE1 = for {
      root <- roots1
      shortest = sentenceE1.dependencies.get.shortestPathEdges(e1Int, root, ignoreDirection = true)
      path <- shortest
      //_ = println(s"root: $root PATH: ${path.mkString(";")}")
    } yield (root, path, path.length)
    val root1AndPath = pathsForRootsToE1.filter(path => path._3 > 0)//
    val (root1, shortestE1ToRoot) = if (root1AndPath.isEmpty) {
      // e1 is the root!
      (e1Int, Seq.empty[(Int, Int, String, String)])
    } else {
      (root1AndPath.minBy(_._3)._1, root1AndPath.minBy(_._3)._2)
    }


    //println("E2 PATH HUNTING:")
    val pathsForRootsToE2 = for {
      root <- roots2
      shortest = sentenceE2.dependencies.get.shortestPathEdges(root, e2Int, ignoreDirection = true)
      path <- shortest
      //_ = println(s"root: $root PATH: ${path.mkString(";")}")
    } yield (root, path, path.length)
    val root2AndPath = pathsForRootsToE2.filter(path => path._3 > 0)
    val (root2, shortestRootToE2) = if (root2AndPath.isEmpty) {
      // e1 is the root!
      (e2Int, Seq.empty[(Int, Int, String, String)])
    } else {
      (root2AndPath.minBy(_._3)._1, root2AndPath.minBy(_._3)._2)
    }

    val path = lexicalizedPathGovHeadBackoff(sentenceE1.words, sentenceE2.words, root1, root2, shortestE1ToRoot, shortestRootToE2)

//    val shortestE1ToRoot = sentenceE1.dependencies.get.shortestPathEdges(e1Int, root1, ignoreDirection = true)
    //val shortestRootToE2 = sentenceE2.dependencies.get.shortestPathEdges(root2, e2Int, ignoreDirection = true)

//    println(s"len of shortestE1ToRoot: ${shortestE1ToRoot.length}")
//    println(s"len of shortestRootToE2: ${shortestRootToE2.length}")

    val toPrint = (fields.slice(0, 5) ++ Seq(path)).mkString("\t")
    pw.println(toPrint)
    //println(s"${sentence.getSentenceText()}")
    //println(s"\t${sentence.words.zipWithIndex.mkString(", ")}")
    println(s"\te1: ${e1} ($e1Int)\te2: ${e2} ($e2Int) --> \n\t\theadlexPath: \t${path}")



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
  def lexicalizedPathWithPOS(words: Seq[String], tags: Seq[String], path: Seq[(Int, Int, String, String)]): String = {
    val out = new ArrayBuffer[String]
    val start = path.head

    val leftWord = if (start._4 == ">") words(start._1) else words(start._2)
    val rightWord = if (start._4 == ">") words(start._2) else words(start._1)

    val leftTag = if (start._4 == ">") tags(start._1) else tags(start._2)
    val rightTag = if (start._4 == ">") tags(start._2) else tags(start._1)


    val startRel = start._4 + start._3

    out.append(s"$leftWord $leftTag $startRel $rightWord $rightTag")

    for (edge <- path.slice(1, path.length)) {
      val rightWord = if (edge._4 == ">") words(edge._2) else words(edge._1)
      val rightTag = if (edge._4 == ">") tags(edge._2) else tags(edge._1)
      out.append(s"${edge._4}${edge._3} $rightWord $rightTag")
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

  // from, to, relation, direction
  def lexicalizedPathGovHeadBackoff(words1: Seq[String],
                                    words2: Seq[String],
                                    headInt1: Int,
                                    headInt2: Int,
                                    pathe1ToHead: Seq[(Int, Int, String, String)],
                                    pathHeadToE2: Seq[(Int, Int, String, String)]): String = {

    val out = new ArrayBuffer[String]

    // Case 1 -- both paths are empty
    if (pathe1ToHead.isEmpty && pathHeadToE2.isEmpty) {
      // shouldn't happen becuase the head should be only one or the other
//      throw new RuntimeException("Both of the shortest paths are empty!")
      //todo both entiies are roots?
      return s"${words1(headInt1)} ${words2(headInt2)}"
    }

    // Check to make sure e1 isn't the head (i.e. there is a path)
    if (pathe1ToHead.nonEmpty) {
      //println(s"PATH E1 to HEAD: ${pathe1ToHead.mkString(";")}")
      val start = pathe1ToHead.head
      val leftWord = if (start._4 == ">") words1(start._1) else words1(start._2)
      //println(s"leftWord: $leftWord")
      val rightWord = if (start._4 == ">") words1(start._2) else words1(start._1)
      //println(s"rightWord: $rightWord")
      val startRel = start._4 + start._3
      out.append(s"$leftWord $startRel")

      val restOfPath = pathe1ToHead.slice(1, pathe1ToHead.length)
      if (restOfPath.nonEmpty) {
        //println("** rest of the path:")
        var lastWord = ""
        for (edge <- restOfPath) {
          val rightWord = if (edge._4 == ">") words1(edge._2) else words1(edge._1)
          //println(s"rightWord: $rightWord")
          val pathPiece = s"${edge._4}${edge._3}"
          //println(s"pathPiece: $pathPiece")
          out.append(pathPiece)
          //println(s"appended: ( ${edge._4}${edge._3} )")
          lastWord = rightWord
        }
        // I think we land on the governing head, but check
        //println(s"lastWord: $lastWord\tgovHead: ${words(headInt)}")
        assert(lastWord == words1(headInt1))
        out.append(words1(headInt1))
      } else {
        out.append(words1(headInt1))
      }
    } else {
      out.append(words1(headInt1))
    }

    out.append("<CROSS_SENT>")

    // Check to make sure e2 isn't the head (i.e., there is a path)
    if (pathHeadToE2.nonEmpty) {
      //println(s"PATH HEAD to E2: ${pathHeadToE2.mkString(";")}")
      // I think that the head will be the first thing here, and the path will follow to e2, but check
      val starte2 = pathHeadToE2.head
      //println(s"starte2: $starte2")
      assert(starte2._1 == headInt2 || starte2._2 == headInt2)
      out.append(words2(headInt2))


      var lastWord = ""
      for (edge <- pathHeadToE2) {
        val rightWord = if (edge._4 == ">") words2(edge._2) else words2(edge._1)
        out.append(s"${edge._4}${edge._3}")
        lastWord = rightWord
      }
      out.append(lastWord)
    } else {
      // no path means that the root of sentence 2 is the entity:
      out.append(words2(headInt2))
    }

    val output = out.mkString(" ")
    //println (s"aggregation output: ${output}")
    output

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

  def findFilesSuffix(collectionDir: String, suffix: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(suffix)
    }

    val result = dir.listFiles(filter)
    if (result == null)
      throw Sourcer.newFileNotFoundException(collectionDir)
    result
  }

  def countUnique(file:File): Unit = {
    val unique = scala.collection.mutable.Set[String]()
    val source = scala.io.Source.fromFile(file)
    // todo: make a buffered reader??? (for memory issues)
    val lines = source.getLines()
    //var lineCounter = 0
    while (lines.hasNext) {
      val line = lines.next()
      unique.add(line)
    }
    println(s"There are ${unique.size} unique lines!")
  }

  val nCores = 40

//  val outputSuffix = ".deps.headLex"
//  val mode: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit = writeLexicalizeOnlyGovHead

//  val outputSuffix = ".deps.fullyLex"
//  val mode: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit = writeFullyLexicalized

  val outputSuffix = ".deps.fullyLex.pos"
  val mode: (Sentence, Seq[String], Int, Int, String, String, PrintWriter) => Unit = writeFullyLexicalizedWithPOS


  //val dir = "/work/bsharp/relationExtraction/gids/raw/may20tmp"
  val dir = "/work/bsharp/relationExtraction/RE/fan/chunked_train"

  //val files = findFilesSuffix(dir, ".txt").par
  val files = findFilesPrefix(dir, "x").par

  files.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nCores))
  for {
    file <- files
  } loadInstances(file, outputSuffix, mode)


}
