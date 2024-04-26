package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.{DocReference, IndexField}
import com.joliciel.jochre.search.core.db.PostgresDatabase.getClass
import com.joliciel.jochre.search.core.text.SynonymMapReader
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.TermQuery
import org.apache.lucene.store.{Directory, FSDirectory, SingleInstanceLockFactory}
import org.slf4j.LoggerFactory
import zio.{ZIO, ZLayer}

import java.nio.file.Path
import scala.jdk.CollectionConverters._

case class JochreIndex(indexDirectory: Directory, analyzerGroup: AnalyzerGroup) {
  val analyzerPerField: Map[String, Analyzer] = Map(
    IndexField.Text.entryName -> analyzerGroup.forIndexing
  )

  val indexAnalyzer: PerFieldAnalyzerWrapper =
    new PerFieldAnalyzerWrapper(analyzerGroup.forIndexingFields, analyzerPerField.asJava)

  private val config = new IndexWriterConfig(indexAnalyzer)
  private val log = LoggerFactory.getLogger(getClass)

  log.info(f"Opening index at $indexDirectory")

  config.setOpenMode(OpenMode.CREATE_OR_APPEND)
  private val indexWriter = new IndexWriter(indexDirectory, config)

  val searcherManager: JochreSearcherManager = new JochreSearcherManager(indexWriter, analyzerGroup)

  def refresh: Boolean = searcherManager.refreshIndex()

  def deleteDocument(docRef: DocReference): Unit =
    indexWriter.deleteDocuments(new TermQuery(new Term(IndexField.Reference.entryName, docRef.ref)))

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
