package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._

class TestEidosProcessor extends ExtractionTest {

  behavior of "document filter"

  it should "not change text with newlines" in {
    val oldText = "This is a sentence\n\nwith newlines."
    val document = ieSystem.annotate(oldText)
    val newText = document.text.get

    newText should be (oldText)
  }

  it should "not change text with form feeds" in {
    val oldText = "This is a sentence\fwith a form feed."
    val document = ieSystem.annotate(oldText)
    val newText = document.text.get

    newText should be (oldText)
  }

  it should "not change end of sentence punctuation" in {
    val oldText = "This is one sentence. "
    val document = ieSystem.annotate(oldText)
    val newText = document.text.get

    newText should be (oldText)
  }

  behavior of "filter headings"

  {
    val io = Seq(
      (". \n \n ", ".\n\n   "), //  0
      (".\n \n ",  ".\n\n  "),  //  1
      (". \n\n ",  ".\n\n  "),  //  2
      (". \n \n",  ".\n\n  "),  //  3
      (".\t\n\n",  ".\n\n "),   //  4
      (".\n\f\n",  ".\n\n "),   //  5
      (".\n\n\r",  ".\n\n "),   //  6
      (".\n\n",    ".\n\n"),    //  7
      (".\n.\n.",  ".\n.\n."),  //  8

      (" \n \n ",  " .\n\n "),  //  9
      ("\n \n ",   " .\n\n"),   // 10
      (" \n\n ",   " .\n\n"),   // 11
      (" \n \n",   " .\n\n"),   // 12
      ("\t\n\n",   " .\n"),     // 13
      ("\n\f\n",   " .\n"),     // 14
      ("\n\n\r",   " .\n"),     // 15
      ("\n\n",     ". "),       // 16 // Which side should be space be on?

      ("\n\n   ",  " .\n\n "),  // 17

      ("abc. \n \n def", "abc.\n\n   def"), // 18
      (". \n \n . \n \n ", ".\n\n   .\n\n   "), // 19
      (". \n \n . abc\n \n ", ".\n\n   . abc .\n\n") // 20
    )
    val proc = ieSystem.components.proc

//    it should "change the text locally" in {
//      io.zipWithIndex.foreach { case ((inputText, outputText), index) =>
//        val filteredText = proc.filterText(inputText)
//        val cleanText = outputText.replace('\n', ' ') // This has been added recently.
//
//        (index, filteredText) should be((index, cleanText))
//      }
//    }

    it should "not change the text globally" in {
      io.zipWithIndex.foreach { case ((inputText, outputText), index) =>
        val oldText = inputText
        val document = ieSystem.annotate(oldText)
        val newText = document.text.get
        val periodSentenceOpt = document.sentences.find { sentence =>
          val periodOpt = sentence.words.find { word =>
            word == "."
          }

          periodOpt.isDefined
        }

        if (document.sentences.nonEmpty)
          periodSentenceOpt.isDefined should be(true)
        newText should be(oldText)
      }
    }
  }

  behavior of "a heading"

  it should "be its own sentence" in {
    val oldText = "Once upon a time\n\nthere were three bears."
    val document = ieSystem.annotate(oldText)

    document.sentences.length should be (2)
  }

  behavior of "raw text"

  it should "match original text even though words don't" in {
    val oldText = "The \u03b1 and \u03c9"
    val document = ieSystem.annotate(oldText)
    val newText = document.text.get
    val rawText = document.sentences(0).raw.mkString(" ")
    val alpha = document.sentences(0).words(1)
    val omega = document.sentences(0).words(3)

    alpha should be ("alpha")
    omega should be ("omega")
    newText should be (oldText)
    newText should be (rawText)
  }

  it should "match original punctuation even though words don't" in {
    val oldText = "Header\n\nParagraph"
    val document = ieSystem.annotate(oldText)
    val newText = document.text.get
    val rawText = document.sentences.flatMap(_.raw).mkString(" ")
    val period = document.sentences(0).words(1)

    period should be (".")
//    rawText.indexOf(".") should be (-1) // This will fail for DocumentFilter version
    newText should be (oldText)
  }
}
