package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference
import org.apache.lucene.index.{IndexReader, Term}
import org.apache.lucene.search.{IndexSearcher, MatchAllDocsQuery, Query, TermQuery, TopDocs, Sort => LuceneSort}
import org.slf4j.LoggerFactory

import scala.collection.compat.immutable.ArraySeq
import scala.util.Using

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
    val topDocs = search(new TermQuery(new Term(LuceneField.Id.name, docRef.ref)), 1)
    topDocs.scoreDocs.headOption.map(luceneId => new LuceneDocument(this, luceneId.doc))
  }

  def getLuceneDocument(luceneId: Int): LuceneDocument = new LuceneDocument(this, luceneId)

  def findMatchingRefs(query: SearchQuery, maxDocs: Int = Int.MaxValue): Seq[DocReference] = {
    val luceneQuery = query.toLuceneQuery(this)
    if (log.isDebugEnabled) log.debug(f"query: $luceneQuery")
    val topDocs = this.search(luceneQuery, maxDocs)
    val luceneDocs = this.getLuceneDocs(topDocs)
    luceneDocs.map(_.ref)
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
}
