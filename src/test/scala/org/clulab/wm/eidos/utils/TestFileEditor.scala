package org.clulab.wm.eidos.utils

import java.io.File

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidoscommon.utils.FileEditor

class TestFileEditor extends Test {
  
  behavior of "FileBuilder"
  
  it should "change an extension" in {
    val expectedFile = new File("dir/subdir/file.jsonld")
    val oldFile = new File("dir/subdir/file.txt")
    val newFile = FileEditor(oldFile).setExt("jsonld").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "change a name" in {
    val expectedFile = new File("dir/subdir/newfile.jsonld")
    val oldFile = new File("dir/subdir/file.txt")
    val newFile = FileEditor(oldFile).setName("newfile.jsonld").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "append to a dir" in {
    val expectedFile = new File("dir/subdir/subdir2/file.txt")
    val oldFile = new File("dir/subdir/file.txt")
    val newFile = FileEditor(oldFile).incDir("subdir2").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "append to a name" in {
    val expectedFile = new File("dir/subdir/file.txt.serialized")
    val oldFile = new File("dir/subdir/file.txt")
    val newFile = FileEditor(oldFile).incName(".serialized").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "change a pathless name" in {
    val expectedFile = new File("newfile.jsonld")
    val oldFile = new File("file.txt")
    val newFile = FileEditor(oldFile).setName("newfile.jsonld").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "change a directory" in {
    val expectedFile = new File("newdir/newsubdir/file.txt")
    val oldFile = new File("dir/subdir/file.txt")
    val newFile = FileEditor(oldFile).setDir("newdir/newsubdir").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }

  it should "change a pathless directory" in {
    val expectedFile = new File("newdir/newsubdir/file.txt")
    val oldFile = new File("file.txt")
    val newFile = FileEditor(oldFile).setDir("newdir/newsubdir").get

    newFile.getCanonicalPath should be (expectedFile.getCanonicalPath)
  }
}