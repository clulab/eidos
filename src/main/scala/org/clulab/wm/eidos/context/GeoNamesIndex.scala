package org.clulab.wm.eidos.context

import java.nio.file.Paths
import java.util.regex.Pattern

import scala.collection.JavaConverters._

import org.apache.lucene.analysis.{Analyzer, LowerCaseFilter}
import org.apache.lucene.analysis.core.{KeywordAnalyzer, KeywordTokenizer}
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.ngram.NGramTokenFilter
import org.apache.lucene.analysis.pattern.PatternReplaceFilter
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.document.{Document, Field, StoredField, StringField, TextField}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, Query, TermQuery}
import org.apache.lucene.search.grouping.GroupingSearch
import org.apache.lucene.store.FSDirectory
import org.clulab.wm.eidos.utils.Sourcer

object GeoNamesIndexConfig {
  val idField: String => Field = new StoredField("id", _)
  val nameField: String => Field = new TextField("name", _, Field.Store.NO)
  val ngramsField: String => Field = new TextField("ngrams", _, Field.Store.NO)
  val latitudeField: Float => Field = new StoredField("latitude", _)
  val longitudeField: Float => Field = new StoredField("longitude", _)
  val populationField: Long => Field = new StoredField("population", _)
  val idEndField: Field = new StringField("idEnd", "x", Field.Store.NO)

  val idEndQuery: Query = new TermQuery(new Term("idEnd", "x"))
  val nameAnalyzer: Analyzer = (_: String) => {
    val tokenizer = new KeywordTokenizer
    val filter = new PatternReplaceFilter(new LowerCaseFilter(tokenizer), Pattern.compile("\\W+"), "", true)
    new Analyzer.TokenStreamComponents(tokenizer, filter)
  }
  val ngramAnalyzer: Analyzer = (_: String) => {
    val tokenizer = new StandardTokenizer
    val filter = new NGramTokenFilter(new LowerCaseFilter(tokenizer), 2, 2)
    new Analyzer.TokenStreamComponents(tokenizer, filter)
  }
  val analyzer: Analyzer = new PerFieldAnalyzerWrapper(
    new KeywordAnalyzer, Map("name" -> nameAnalyzer, "ngrams" -> ngramAnalyzer).asJava)
}

object IndexGeoNames {
  def main(args: Array[String]): Unit = args match {
    case Array(geoNamesPath, indexPath) =>
      // create an index writer
      val dir = FSDirectory.open(Paths.get(indexPath))
      val config = new IndexWriterConfig(GeoNamesIndexConfig.analyzer)
      config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
      val writer = new IndexWriter(dir, config)

      // walk through each line of the GeoNames file
      for (line <- Sourcer.sourceFromFile(geoNamesPath).getLines) {
        val Array(geoNameID, name, asciiName, alternateNames, latitude, longitude,
        _, _, _, _, _, _, _, _, population, _, _, _, _) = line.split("\t")

        // generate a document for each name of this ID
        val docs = for (name <- Array(name, asciiName) ++ alternateNames.split(",")) yield {
          val doc = new Document
          doc.add(GeoNamesIndexConfig.idField(geoNameID))
          doc.add(GeoNamesIndexConfig.nameField(name))
          doc.add(GeoNamesIndexConfig.ngramsField(name))
          doc.add(GeoNamesIndexConfig.latitudeField(latitude.toFloat))
          doc.add(GeoNamesIndexConfig.longitudeField(longitude.toFloat))
          doc.add(GeoNamesIndexConfig.populationField(population.toLong))
          doc
        }

        // mark the last document (name) for the ID
        docs.last.add(GeoNamesIndexConfig.idEndField)

        // write all documents (names) for this ID in a block
        val docsList: java.util.List[Document] = java.util.Arrays.asList(docs: _*)
        writer.addDocuments(docsList)
      }
      writer.close()
  }
}

object SearchGeoNames {
  def main(args: Array[String]): Unit = args.toList match {
    case indexPath :: queryStrings =>
      val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexPath)))
      val searcher = new IndexSearcher(reader)
      val groupingSearch = new GroupingSearch(GeoNamesIndexConfig.idEndQuery)
      val queryParser = new QueryParser("ngrams", GeoNamesIndexConfig.analyzer)
      for (queryString <- queryStrings) {
        val escapedQueryString = queryString.replaceAll("\\s", "\\\\ ")
        val query = queryParser.parse(f"name:$escapedQueryString ngrams:($queryString)")
        println(query)
        val results = groupingSearch.search(searcher, query, 0, 5)
        for (group <- results.groups) {
          val id = group.scoreDocs.head.doc
          val doc = searcher.doc(id)
          val geoNamesID = doc.get("id")
          println(f"${group.maxScore}%.3f $geoNamesID")
        }
      }
      reader.close()
  }
}

