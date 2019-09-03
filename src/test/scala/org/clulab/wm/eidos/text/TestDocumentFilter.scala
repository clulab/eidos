package org.clulab.wm.eidos.text

import org.clulab.wm.eidos.test.TestUtils._

class TestDocumentFilter extends ExtractionTest {

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
}
