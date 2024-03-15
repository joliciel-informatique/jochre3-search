package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.index.{IndexReader, IndexWriter}
import org.apache.lucene.search.{SearcherFactory, SearcherManager}

/** Wrapper for [[SearcherManager]] to expose searcher as [[JochreSearcher]].
  */
private[lucene] class JochreSearcherManager(indexWriter: IndexWriter, analyzerGroup: AnalyzerGroup) {

  private val searcherFactory = new JochreSearcherFactory(this, analyzerGroup)
  private val searcherManager = new SearcherManager(indexWriter, searcherFactory)

  def acquire(): JochreSearcher = searcherManager.acquire().asInstanceOf[JochreSearcher]
  def release(jochreSearcher: JochreSearcher): Unit = searcherManager.release(jochreSearcher)
  def refreshIndex(): Boolean = searcherManager.maybeRefresh()
}

private class JochreSearcherFactory(searcherManager: JochreSearcherManager, analyzerGroup: AnalyzerGroup)
    extends SearcherFactory {
  override def newSearcher(reader: IndexReader, previousReader: IndexReader): JochreSearcher =
    new JochreSearcher(reader, searcherManager, analyzerGroup)
}
