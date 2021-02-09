package org.clulab.wm.eidoscommon.utils

class TestString extends Test {
  
  behavior of "String"
  
  it should "not be automatically stripped" in {
    val string = """
      |Up to 4.8 million people in South Sudan - well over one-third of the
      |population - will be facing severe food shortages over the coming months, and the risk of a
      |hunger catastrophe continues to threaten parts of the country, three UN agencies warned
      |today.      
      """
    
    string.contains('|') should be (true)
    string.stripMargin.contains('|') should be (false)
  }  
}
