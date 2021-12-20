package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.ExtractionTest

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

  it should "remove sentences with too many numbers or single char words" in {
    val text = "15.0 20.0 25.0 30.0 35.0 40.0 45.0 Ja n -1 7 A p r1 7 Ju l1 7 O ct -1 7 Ja n -1 8 A p r1 8 Ju l1 8 O ct -1 8 Ja n -1 9 A p r1 9 Ju l1 9 O ct -1 9 Ja n -2 0 A p r2 0 Ju l2 0 Price trend of teffin Addis Ababa market Teff' white Teff' mixed Maize prices reduced or remained stable since June through August in most markets following harvests in unimodal areas but also increased imports from Uganda and Tanzania after the Government issued permits for private companies to import about 4 million bags of maize."
    val document = ieSystem.annotate(text)
    document.sentences.length should be (0)
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
      (". \n \n . abc\n \n ", ".\n\n   . abc .\n\n"), // 20
      ("sentence", "sentence") // 21
    )

    it should "change the text locally" in {
      io.zipWithIndex.foreach { case ((inputText, _), _) =>
        val oldText = inputText
        val document = ieSystem.annotate(oldText)
        val newText = document.text.get

        oldText should be (newText)
        document.sentences.foreach { sentence =>
          sentence.words.last should be (".")
        }
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
    val oldText = "The \u03b1 and \u03c9 ."
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
    val period = document.sentences(0).words(1)

    period should be (".")
    newText should be (oldText)
  }
}
