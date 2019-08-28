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
      (". \n \n ", ".\n\n   "),
      (".\n \n ",  ".\n\n  "),
      (". \n\n ",  ".\n\n  "),
      (". \n \n",  ".\n\n  "),
      (".\t\n\n",  ".\n\n "),
      (".\n\f\n",  ".\n\n "),
      (".\n\n\r",  ".\n\n "),
      (".\n\n",    ".\n\n"),
      (".\n.\n.",  ".\n.\n."),

      (" \n \n ",  " .\n\n "),
      ("\n \n ",   " .\n\n"),
      (" \n\n ",   " .\n\n"),
      (" \n \n",   " .\n\n"),
      ("\t\n\n",   " .\n"),
      ("\n\f\n",   " .\n"),
      ("\n\n\r",   " .\n"),
      ("\n\n",     ". "), // Which side should be space be on?

      ("\n\n   ",  " .\n\n "), // The last \s* needs to be greedy!

      ("abc. \n \n def", "abc.\n\n   def"),
      (". \n \n . \n \n ", ".\n\n   .\n\n   "),
      (". \n \n . abc\n \n ", ".\n\n   . abc .\n\n")
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
