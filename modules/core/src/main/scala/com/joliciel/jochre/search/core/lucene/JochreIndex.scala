package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference
import com.typesafe.config.ConfigFactory
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.{Directory, FSDirectory, SingleInstanceLockFactory}
import zio.{Task, ZIO, ZLayer}

import java.nio.file.Path

case class JochreIndex(indexDirectory: Directory, analyzerGroup: AnalyzerGroup) {
  private val config = new IndexWriterConfig(analyzerGroup.forIndexing)
  config.setOpenMode(OpenMode.CREATE_OR_APPEND)
  private val indexWriter = new IndexWriter(indexDirectory, config)

  val searcherManager: JochreSearcherManager = new JochreSearcherManager(indexWriter, analyzerGroup)

  def refresh: Boolean = searcherManager.refreshIndex()

  def indexer: LuceneDocIndexer = LuceneDocIndexer(indexWriter)

  def addDocumentInfo(ref: DocReference, docInfo: DocumentIndexInfo): Unit =
    analyzerGroup.forIndexing.indexingHelper.addDocumentInfo(ref, docInfo)

  def removeDocumentInfo(ref: DocReference): Unit =
    analyzerGroup.forIndexing.indexingHelper.removeDocumentInfo(ref)

  def commit: Unit = indexer.commit()

  private[search] def delete: Unit = {
    indexWriter.deleteAll()
  }
}

object JochreIndex {
  private val config = ConfigFactory.load().getConfig("jochre.search.index")
  def fromConfig: JochreIndex = {
    val indexPath = Path.of(config.getString("directory"))
    val indexDirectory = FSDirectory.open(indexPath, new SingleInstanceLockFactory)
    val analyzerGroup = AnalyzerGroup.generic
    JochreIndex(indexDirectory, analyzerGroup)
  }

  val live: ZLayer[Any, Throwable, JochreIndex] = ZLayer.fromZIO(ZIO.attempt { fromConfig })
}
