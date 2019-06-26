package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern

import de.bwaldvogel.liblinear.{Feature, FeatureNode, Linear, Model, Parameter, Problem, SolverType}

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


class GeoNamesEntry(document: Document) {
  lazy val id: String = document.get("id")
  lazy val name: String = document.get("canonical-name")
  lazy val featureCode: String = document.get("feature-code")
  lazy val population: Long = document.get("population").toLong
}


object GeoNamesIndexConfig {
  val idField: String => Field = new StoredField("id", _)
  val canonicalNameField: String => Field = new StoredField("canonical-name", _)
  val nameField: String => Field = new TextField("name", _, Field.Store.NO)
  val ngramsField: String => Field = new TextField("ngrams", _, Field.Store.NO)
  val latitudeField: Float => Field = new StoredField("latitude", _)
  val longitudeField: Float => Field = new StoredField("longitude", _)
  val featureCodeField: String => Field = new StoredField("feature-code", _)
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
    val filter = new NGramTokenFilter(new LowerCaseFilter(tokenizer), 3, 3)
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
        val Array(geoNameID, canonicalName, asciiName, alternateNames, latitude, longitude,
        _, featureCode, _, _, _, _, _, _, population, _, _, _, _) = line.split("\t")

        // generate a document for each name of this ID
        val docs = for (name <- Array(canonicalName, asciiName) ++ alternateNames.split(",")) yield {
          val doc = new Document
          doc.add(GeoNamesIndexConfig.idField(geoNameID))
          doc.add(GeoNamesIndexConfig.canonicalNameField(canonicalName))
          doc.add(GeoNamesIndexConfig.nameField(name))
          doc.add(GeoNamesIndexConfig.ngramsField(name))
          doc.add(GeoNamesIndexConfig.latitudeField(latitude.toFloat))
          doc.add(GeoNamesIndexConfig.longitudeField(longitude.toFloat))
          doc.add(GeoNamesIndexConfig.featureCodeField(featureCode))
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


class GeoNamesSearcher(indexPath: Path) {
  private val reader = DirectoryReader.open(FSDirectory.open(indexPath))
  private val searcher = new IndexSearcher(reader)
  private val groupingSearch = new GroupingSearch(GeoNamesIndexConfig.idEndQuery)
  private val nameQueryParser = new QueryParser("name", GeoNamesIndexConfig.analyzer)
  private val ngramsQueryParser = new QueryParser("ngrams", GeoNamesIndexConfig.analyzer)

  def apply(queryString: String, maxFuzzyHits: Int): Seq[(GeoNamesEntry, Float)] = {
    // escape special characters for queries to "name" field
    val luceneSpecialCharacters = """([-+&|!(){}\[\]^"~*?:\\/\s])"""
    val escapedQueryString = queryString.replaceAll(luceneSpecialCharacters, """\\$1""")

    // first look for an exact match of the input phrase (the "name" field ignores spaces, punctuation, etc.)
    var results = scoredEntries(nameQueryParser.parse(escapedQueryString), 1000)

    // if there's no exact match, search for fuzzy (1-2 edit-distance) matches
    if (results.isEmpty) {
      results = scoredEntries(nameQueryParser.parse(escapedQueryString + "~"), maxFuzzyHits)
    }
    // if there's no fuzzy match, search for n-gram matches
    if (results.isEmpty) {
      results = scoredEntries(ngramsQueryParser.parse(queryString), maxFuzzyHits)
    }
    // sort first by retrieval score, then by population, then by feature code (e.g., ADM1 before ADM3 and PPL)
    results.sortBy{
      case (entry, score) => (-score, -math.log10(entry.population + 1).round, entry.featureCode)
    }
  }

  def scoredEntries(query: Query, maxHits: Int): Array[(GeoNamesEntry, Float)] = {
    // perform a group-based search, where each group represents all names for a GeoNames ID
    val topGroups = groupingSearch.search[String](searcher, query, 0, maxHits)

    // for each of the hits, return an object representing the GeoNames entry, and the retrieval score
    if (topGroups == null) Array.empty else {
      for (group <- topGroups.groups) yield {
        val headDoc = searcher.doc(group.scoreDocs.head.doc)
        (new GeoNamesEntry(headDoc), group.maxScore)
      }
    }
  }

  def close(): Unit = {
    reader.close()
  }
}


object SearchGeoNames {
  def main(args: Array[String]): Unit = args match {
    case Array(indexPath, queryStrings @ _*) =>
      val searcher = new GeoNamesSearcher(Paths.get(indexPath))
      for (queryString <- queryStrings) {
        println(queryString)
        for ((entry, score) <- searcher(queryString, 20)) {
          println(f"$score%.3f ${entry.id} ${entry.name} ${entry.featureCode} ${entry.population}")
        }
      }
      searcher.close()
  }
}


class GeoNamesReranker(searcher: GeoNamesSearcher, linearModel: Option[Model] = None) {

  private def searchEachSpan(text: String, spans: Seq[(Int, Int)]): Seq[Seq[(GeoNamesEntry, Float)]] = {
    for ((start, end) <- spans) yield searcher(text.substring(start, end), 5)
  }

  def entryFeatures(text: String, spans: Seq[(Int, Int)]): Seq[Seq[(GeoNamesEntry, Array[Feature])]] = {
    for (entryScores <- this.searchEachSpan(text, spans)) yield {
      val scores = entryScores.map(_._2).distinct.sorted.reverse
      val scoreRanks = scores.zipWithIndex.toMap
      for ((entry, score) <- entryScores) yield {
        val logPopulation = math.log10(entry.population + 1)
        val features = Array[Feature](
          new FeatureNode(1, scoreRanks(score)),
          new FeatureNode(2, logPopulation),
        ) ++ GeoNamesReranker.featureCodeToIndex.get(entry.featureCode).map(new FeatureNode(_, logPopulation))
        (entry, features)
      }
    }
  }

  def apply(text: String, spans: Seq[(Int, Int)]): Seq[Seq[(GeoNamesEntry, Float)]] = {
    linearModel match {
      // if there is no model, rely on the searcher's order
      case None => this.searchEachSpan(text, spans)
      // if there is a model, rerank the search results
      case Some(model) =>
        val indexOf1 = model.getLabels.indexOf(1)
        this.entryFeatures(text, spans).map(_.map {
          case (entry, features) =>
            (entry, Linear.predict(model, features).toFloat)
            // Below uses probabilities as scores, but results in worse performance
            // val probs = Array(0.0, 0.0)
            // Linear.predictProbability(model, features, probs)
            // (entry, probs(indexOf1).toFloat)
        }.sortBy(-_._2))
    }
  }

  def save(modelPath: Path): Unit = {
    for (model <- linearModel) {
      val writer = Files.newBufferedWriter(modelPath)
      model.save(writer)
      writer.close()
    }
  }
}


object GeoNamesReranker {

  // country, state, region, ... and city, village, ... from http://www.geonames.org/export/codes.html
  private val featureCodeToIndex: Map[String, Int] =
    """
    ADM1 ADM1H ADM2 ADM2H ADM3 ADM3H ADM4 ADM4H ADM5 ADMD ADMDH LTER PCL PCLD PCLF PCLH PCLI PCLIX PCLS PRSH TERR ZN ZNB
    PPL PPLA PPLA2 PPLA3 PPLA4 PPLC PPLCH PPLF PPLG PPLH PPLL PPLQ PPLR PPLS PPLW PPLX STLMT
    """.split("""\s+""").zipWithIndex.map{ case (code, i) => (code, i + 3) }.toMap

  def load(geoNamesIndexPath: Path, modelPath: Path): GeoNamesReranker = {
    val searcher = new GeoNamesSearcher(geoNamesIndexPath)
    val reader = Files.newBufferedReader(modelPath)
    val model = Model.load(reader)
    new GeoNamesReranker(searcher, Some(model))
  }

  def train(geoNamesIndexPath: Path,
            trainingData: Iterator[(String, Seq[(Int, Int)], Seq[String])]): GeoNamesReranker = {
    val searcher = new GeoNamesSearcher(geoNamesIndexPath)
    val reranker = new GeoNamesReranker(searcher)
    val featureLabels: Iterator[(Array[Feature], Double)] = for {
      (text, spans, geoIDs) <- trainingData
      (entryFeatures, geoID) <- reranker.entryFeatures(text, spans) zip geoIDs
      (entry, features) <- entryFeatures
    } yield {
      (features, if (entry.id == geoID) 1.0 else 0.0)
    }
    val (features, labels) = featureLabels.toArray.unzip
    val problem = new Problem
    problem.x = features
    problem.y = labels
    problem.l = labels.length
    problem.n = featureCodeToIndex.values.max + 2
    problem.bias = 1
    val param = new Parameter(SolverType.L1R_LR, 1.0, 0.01)
    val model = Linear.train(problem, param)
    new GeoNamesReranker(searcher, Some(model))
  }
}