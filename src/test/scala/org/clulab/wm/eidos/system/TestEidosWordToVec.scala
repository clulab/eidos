package org.clulab.wm.eidos.system

import org.clulab.embeddings.CompactWordEmbeddingMap
import org.clulab.embeddings.ExplicitWordEmbeddingMap
import org.clulab.wm.eidos.test.Test

class TestEidosWordToVec extends Test {

  behavior of "CompactWordEmbeddingMap types"

  it should "work for Array" in {
    val length = 3
    val implArray = new CompactWordEmbeddingMap.ImplArrayType(length)
    val array = implArray.asInstanceOf[Array[Float]]

    println(array(length -1))
  }

  it should "work for Seq" in {
    val length = 3
    val implArray = new CompactWordEmbeddingMap.ImplArrayType(length)
    val implSeq: CompactWordEmbeddingMap.ImplSeqType = implArray
    val seq = implSeq.asInstanceOf[Seq[Float]]

    println(seq(length -1))
  }

  behavior of "ExplicitWordEmbeddingMap types"

  it should "work for Array" in {
    val length = 3
    val implArray = new ExplicitWordEmbeddingMap.ImplArrayType(length)
    val array = implArray.asInstanceOf[Array[Float]]

    println(array(length -1))
  }

  it should "work for Seq" in {
    val length = 3
    val implArray = new ExplicitWordEmbeddingMap.ImplArrayType(length)
    val implSeq: ExplicitWordEmbeddingMap.ImplSeqType = implArray
    val seq = implSeq.asInstanceOf[Seq[Float]]

    println(seq(length -1))
  }
}
