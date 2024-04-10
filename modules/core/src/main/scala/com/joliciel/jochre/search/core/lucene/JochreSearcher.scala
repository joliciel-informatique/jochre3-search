package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.service.{SearchResponse, SearchResult}
import com.joliciel.jochre.search.core.{AggregationBin, DocReference, IndexField, SearchCriterion, SearchQuery}
import org.apache.lucene.facet.FacetsCollector
import org.apache.lucene.facet.sortedset.{DefaultSortedSetDocValuesReaderState, SortedSetDocValuesFacetCounts}
import org.apache.lucene.index.{IndexReader, Term}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{
  BooleanQuery,
  IndexSearcher,
  MatchAllDocsQuery,
  PrefixQuery,
  Query,
  TermQuery,
  TopDocs,
  TopScoreDocCollectorManager,
  Sort => LuceneSort
}
import org.slf4j.LoggerFactory

import scala.collection.compat.immutable.ArraySeq

/** If retrieved with a [[JochreSearcherManager]], should give an immutable view of the index.
  */
private[lucene] class JochreSearcher(
    indexReader: IndexReader,
    manager: JochreSearcherManager,
    val analyzerGroup: AnalyzerGroup
) extends IndexSearcher(indexReader)
    with AutoCloseable {

  private val log = LoggerFactory.getLogger(getClass)

  private[lucene] def getFirst(sortBy: LuceneSort): Option[LuceneDocument] =
    this
      .search(new MatchAllDocsQuery(), 1, sortBy)
      .scoreDocs
      .map { scoreDoc =>
        this.getLuceneDocument(scoreDoc.doc)
      }
      .headOption

  def indexSize: Int = this.getIndexReader.numDocs

  def getByDocRef(docRef: DocReference): Option[LuceneDocument] = {
    val topDocs = search(new TermQuery(new Term(IndexField.Reference.entryName, docRef.ref)), 1)
    topDocs.scoreDocs.headOption.map(luceneId => new LuceneDocument(this, luceneId.doc))
  }

  def getLuceneDocument(luceneId: Int): LuceneDocument = new LuceneDocument(this, luceneId)

  def findMatchingRefs(query: SearchQuery, maxDocs: Int = Int.MaxValue): Seq[DocReference] = {
    val luceneQuery = toLuceneQuery(query)
    if (log.isDebugEnabled) log.debug(f"query: $luceneQuery")
    val topDocs = this.search(luceneQuery, maxDocs)
    val luceneDocs = this.getLuceneDocs(topDocs)
    luceneDocs.map(_.ref)
  }

  def search(
      query: SearchQuery,
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int]
  ): SearchResponse = {
    val luceneQuery = toLuceneQuery(query)
    if (log.isDebugEnabled) log.debug(f"query: $luceneQuery")
    val maxCount = Math.max(1, indexSize)

    val docCollectorManager = new TopScoreDocCollectorManager(first + max, maxCount)
    val topDocs = this.search(luceneQuery, docCollectorManager)
    if (log.isDebugEnabled) log.debug(f"Found ${topDocs.totalHits} results")

    val page = ArraySeq
      .unsafeWrapArray(topDocs.scoreDocs)
      .drop(first)
      .take(max)
      .map { scoreDoc =>
        getLuceneDocument(scoreDoc.doc) -> scoreDoc.score
      }

    val results = page.map { case (luceneDoc, score) =>
      val snippets = luceneDoc.highlight(luceneQuery, maxSnippets, rowPadding)
      SearchResult(luceneDoc.ref, luceneDoc.rev, luceneDoc.metadata, score, snippets)
    }
    SearchResponse(results, topDocs.totalHits.value)
  }

  private def getLuceneDocsWithScores(topDocs: TopDocs): Seq[(LuceneDocument, Float)] = ArraySeq
    .unsafeWrapArray(topDocs.scoreDocs)
    .map { scoreDoc =>
      getLuceneDocument(scoreDoc.doc) -> scoreDoc.score
    }

  private def getLuceneDocs(topDocs: TopDocs): Seq[LuceneDocument] = ArraySeq
    .unsafeWrapArray(topDocs.scoreDocs)
    .map { scoreDoc =>
      getLuceneDocument(scoreDoc.doc)
    }

  private def getHeadDoc(topDocs: TopDocs): Option[LuceneDocument] = topDocs.scoreDocs.headOption
    .map { score =>
      getLuceneDocument(score.doc)
    }

  override def close(): Unit = manager.release(this)

  private def toLuceneQuery(query: SearchQuery): Query = {
    query.criterion.toLuceneQuery(analyzerGroup)
  }

  def aggregate(searchQuery: SearchQuery, field: IndexField, maxBins: Int): Seq[AggregationBin] = {
    val facetCollector = this.prepareFacetCollector(searchQuery)
    val facets = Option(
      new SortedSetDocValuesFacetCounts(
        new DefaultSortedSetDocValuesReaderState(this.indexReader, FacetConfigHolder.facetsConfig),
        facetCollector
      ).getTopChildren(maxBins, field.entryName)
    )

    facets
      .map(_.labelValues.map { labelAndValue =>
        AggregationBin(labelAndValue.label, labelAndValue.value.intValue())
      }.toSeq)
      .getOrElse(Seq.empty)
  }

  private def prepareFacetCollector(searchQuery: SearchQuery): FacetsCollector = {
    val facetCollector = new FacetsCollector(true)
    val hitCountForSearchResult = 0
    val luceneQuery = this.toLuceneQuery(searchQuery)
    FacetsCollector.search(this, luceneQuery, hitCountForSearchResult, facetCollector)
    facetCollector
  }
}
