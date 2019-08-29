package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._

class TestEidosProcessor extends ExtractionTest {

  behavior of "document filter"

  it should "not change text with newlines" in {
    val oldtext = "This is a sentence\n\nwith newlines."
    val document = ieSystem.annotate(oldtext)
    val newText = document.text.get

    newText should be (oldtext)
  }

  it should "not change text with form feeds" in {
    val oldtext = "This is a sentence\fwith a form feed."
    val document = ieSystem.annotate(oldtext)
    val newText = document.text.get

    newText should be (oldtext)
  }

  it should "not change end of sentence punctuation" in {
    val oldtext = "This is one sentence. "
    val document = ieSystem.annotate(oldtext)
    val newText = document.text.get

    newText should be (oldtext)
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

    it should "change the text locally" in {
      io.zipWithIndex.foreach { case ((inputText, outputText), index) =>
        val filteredText = proc.filterText(inputText)
        val cleanText = outputText.replace('\n', ' ') // This has been added recently.

        (index, filteredText) should be((index, cleanText))
      }
    }

    it should "not change the text globally" in {
      io.zipWithIndex.foreach { case ((inputText, outputText), index) =>
        val oldtext = inputText
        val document = ieSystem.annotate(oldtext)
        val newText = document.text.get
        val periodSentenceOpt = document.sentences.find { sentence =>
          val periodOpt = sentence.words.find { word =>
            word == "."
          }

          periodOpt.isDefined
        }

        periodSentenceOpt.isDefined should be(true)
        newText should be(oldtext)
      }
    }
  }

  behavior of "a heading"

  it should "be its own sentence" in {
    val oldtext = "Once upon a time\n\nthere were three bears."
    val document = ieSystem.annotate(oldtext)

    document.sentences.length should be (2)
  }
}
