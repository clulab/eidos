package org.clulab.wm.eidos.system

import org.clulab.dynet.SeqScorer
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.test.ExtractionTest
class TestGeonorm extends ExtractionTest {

//  val file = new File("/Users/vikasy/SEM_8/RA DARPA/eidos/src/test/scala/org/clulab/wm/eidos/system/ner_result_conll_format.txt")
//  val bw = new BufferedWriter(new FileWriter(file))

  def locations(text: String): Seq[Location] = extractMentions(text).filter(_.label == "Location").flatMap(m => m.attachments.collect{
    case loc: Location => loc
  })

  it should "identify geographical locations" in {
    // The model sometimes generates spurious LOC tags in the padding needed for batch processing.
    // As of Jun 2019, this input produced some such tags, so check that no errors result.
    val text = """
      Fighting erupts in Juba and
      quickly spreads to Jonglei,
      Unity and Upper Nile.
      Parties to the conﬂict sign the Recommitment
      on Humanitarian Matters of the Cessation of
      Hostilities Agreement – including 30 days of
      tranquility – and subsequently the Agreement
      to Resolve the Crisis in South Sudan.
    """
    locations(text).map(_.text) should contain allElementsOf Set("Juba", "Jonglei")
    locations(text).flatMap(_.geoPhraseID.geonameID) should contain ("11550569") // South Sudan
    // (no test for Juba since it's ambiguous between the city and the state)

    // The model sometimes generates I-LOC tags at the beginning of a sentence.
    // As of Jun 2019, this input produced such a tag, so check that no errors result.
//    val text2 = "b. Additional counties in need of some form of protection:"
//    locations(text2) shouldBe empty


    val text3 = """
      Fighting erupts in Juba and
      quickly spreads to Jonglei,
      Unity and Upper Nile.
      Parties to the conﬂict sign the Recommitment
      on Humanitarian Matters of the Cessation of
      Hostilities Agreement – including 30 days of
      tranquility – and subsequently the Agreement
      to Resolve the Crisis in South Sudan.
    """
    locations(text3).map(_.text) should contain allElementsOf Set("Juba", "Jonglei")
    // locations(text3).flatMap(_.geoPhraseID.geonameID) should contain ("11550569") // South Sudan

    var dev_file_sentences = ""
    var dev_file_sentences_labels = ""
    var All_predicted_tok_labels = new ListBuffer[String]()
    var All_gold_tok_labels = new ListBuffer[String]()

    var conll_format_predictions = ()
    val source = Source.fromFile("/Users/vikasy/SEM_8/RA DARPA/eidos/src/test/scala/org/clulab/wm/eidos/system/valid_just_loc.txt")
    for (line <- source.getLines())
      if (line.length==0) {

        var loc_entities = locations(dev_file_sentences).map(_.text)
        var sentence_toks = dev_file_sentences.split(" ")
        val sentence_tok_labels = dev_file_sentences_labels.split(" ")
        // val predicted_tok_labels = List[String]()
        var predicted_tok_labels = new ListBuffer[String]()
        for (lab1 <- sentence_tok_labels){
          predicted_tok_labels += "O"
        }
//        println("before updating with METAL output ", predicted_tok_labels)
//        println("figure out how to print var ", sentence_tok_labels.foreach(print) ) //
        for (ent1 <- loc_entities){
             val splitted_ent = ent1.split(" ")
             // if (splitted_ent.length>1){
             try {
               for ((ent_tok, i) <- splitted_ent.view.zipWithIndex) {
                 if (i == 0) {
                   predicted_tok_labels(sentence_toks.indexOf(ent_tok)) = "B-Location"
                 } else {
                   predicted_tok_labels(sentence_toks.indexOf(ent_tok)) = "I-Location"
                 }
               }
             } catch {
               case i: IndexOutOfBoundsException => {
                 println("Out of index error occurred.")
               }
             }

          if (predicted_tok_labels.length == sentence_tok_labels.length){
            All_predicted_tok_labels ++= predicted_tok_labels
            All_gold_tok_labels ++= sentence_tok_labels
          }
        }

//        for ((tok1, ind1) <- sentence_toks.view.zipWithIndex) {
//          bw.write(tok1.toString + " " + sentence_tok_labels(ind1).toString + " " + predicted_tok_labels(ind1).toString + "\n")
//        }
//        bw.write("\n")
        dev_file_sentences = ""
        dev_file_sentences_labels = ""

      } else
      {
        dev_file_sentences +=   " " + line.split(" ")(0)
        dev_file_sentences_labels += " " + line.split(" ")(1)
      }
    // bw.close()
    source.close()

    // val sc = SeqScorer.f1(All_gold_tok_labels.toIndexedSeq, All_predicted_tok_labels.toIndexedSeq)
    println("the F1 score looks like ", All_gold_tok_labels.toIndexedSeq)
    // write the should condition to check if the F1 score is greater than or equal to a specified threshold.

  }
}
