package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.{DocReference, SearchCriterion, Sort}
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.postgres.circe.jsonb.implicits._
import io.circe.generic.auto._
import zio._
import zio.interop.catz._

import java.time.Instant

private[service] case class SearchRepo(transactor: Transactor[Task]) {
  def upsertIndexedDocument(
      ref: DocReference,
      docRev: DocRev,
      wordSuggestionRev: Option[WordSuggestionRev],
      metadataCorrectionRev: Option[MetadataCorrectionRev],
      reindex: Boolean
  ): Task[Int] =
    sql"""INSERT INTO indexed_document (reference, doc_rev, word_suggestion_rev,  metadata_correction_rev, reindex)
         | VALUES (${ref.ref}, ${docRev.rev}, ${wordSuggestionRev.map(_.rev)}, ${metadataCorrectionRev.map(
      _.rev
    )}, $reindex)
         | ON CONFLICT (reference)
         | DO UPDATE SET doc_rev=${docRev.rev},
         |   word_suggestion_rev=${wordSuggestionRev.map(_.rev)},
         |   metadata_correction_rev=${metadataCorrectionRev.map(_.rev)},
         |   reindex=$reindex
         | """.stripMargin.update.run
      .transact(transactor)

  def insertDocument(ref: DocReference, username: String, ipAddress: Option[String]): Task[DocRev] = {
    for {
      docRev <- insertDocumentInternal(ref, username, ipAddress)
      _ <- upsertIndexedDocument(ref, DocRev(0), None, None, false)
    } yield docRev
  }

  private def insertDocumentInternal(ref: DocReference, username: String, ipAddress: Option[String]): Task[DocRev] =
    sql"""INSERT INTO document (reference, username, ip)
         | VALUES (${ref.ref}, $username, $ipAddress)
         | RETURNING rev
         | """.stripMargin
      .query[DocRev]
      .unique
      .transact(transactor)

  def markForReindex(docRef: DocReference): Task[Int] = {
    sql"""UPDATE indexed_document d1 SET reindex=${true}
          | WHERE reference=${docRef.ref}
       """.stripMargin.update.run
      .transact(transactor)
  }
  def markAllForReindex(): Task[Int] = {
    sql"""UPDATE indexed_document d1 SET reindex=${true}
       """.stripMargin.update.run
      .transact(transactor)
  }

  def deleteDocument(docRev: DocRev): Task[Int] =
    sql"""DELETE FROM document WHERE rev=${docRev.rev}""".stripMargin.update.run
      .transact(transactor)

  def deleteOldRevs(docRef: DocReference): Task[Int] =
    sql"""DELETE FROM document
         | WHERE reference = ${docRef.ref}
         | AND rev < (SELECT MAX(rev) FROM document d2 WHERE d2.reference = ${docRef.ref})
       """.stripMargin.update.run
      .transact(transactor)

  def insertPage(docRev: DocRev, page: Page, offset: Int): Task[PageId] =
    sql"""INSERT INTO page (doc_rev, index, width, height, start_offset)
        | VALUES (${docRev.rev}, ${page.physicalPageNumber}, ${page.width}, ${page.height}, $offset)
        | RETURNING id
        | """.stripMargin
      .query[PageId]
      .unique
      .transact(transactor)

  def insertRow(pageId: PageId, index: Int, rectangle: Rectangle): Task[RowId] =
    sql"""INSERT INTO row(page_id, index, lft, top, width, height)
         | values (${pageId.id}, $index, ${rectangle.left}, ${rectangle.top}, ${rectangle.width}, ${rectangle.height})
         | RETURNING id
       """.stripMargin
      .query[RowId]
      .unique
      .transact(transactor)

  def insertWord(docRev: DocRev, rowId: RowId, offset: Int, hyphenatedOffset: Option[Int], word: Word): Task[WordId] =
    sql"""INSERT INTO word(doc_rev, row_id, start_offset, end_offset, hyphenated_offset, lft, top, width, height)
         | values (${docRev.rev}, ${rowId.id}, $offset, ${offset + word.content.length}, $hyphenatedOffset, ${word.left}, ${word.top}, ${word.width}, ${word.height})
         | RETURNING id
       """.stripMargin
      .query[WordId]
      .unique
      .transact(transactor)

  implicit val searchCriterionMeta: Meta[SearchCriterion] = new Meta(pgDecoderGet, pgEncoderPut)
  implicit val sortMeta: Meta[Sort] = new Meta(pgDecoderGet, pgEncoderPut)
  def insertQuery(
      username: String,
      ipAddress: Option[String],
      criteria: SearchCriterion,
      sort: Sort,
      first: Int,
      max: Int,
      resultCount: Int
  ): Task[QueryId] = {
    val query = criteria.getContains().map(_.queryString)
    sql"""INSERT INTO query(username, ip, criteria, query, sort, first_result, max_result, result_count)
         | values ($username, $ipAddress, $criteria, $query, $sort, $first, $max, $resultCount)
         | RETURNING id
       """.stripMargin
      .query[QueryId]
      .unique
      .transact(transactor)
  }

  def getDocument(ref: DocReference): Task[DbDocument] =
    sql"""SELECT rev, reference, username, ip, created
         | FROM document d1
         | WHERE d1.reference = ${ref.ref}
         | AND d1.rev = (SELECT MAX(rev) FROM document d2 WHERE d2.reference = d1.reference)
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getDocument(docRev: DocRev): Task[DbDocument] =
    sql"""SELECT rev, reference, username, ip, created
         | FROM document
         | WHERE document.rev = ${docRev.rev}
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getDocumentsToReindex(): Task[Seq[DocReference]] = {
    sql"""SELECT indexdoc.reference
         | FROM indexed_document AS indexdoc
         | WHERE EXISTS (SELECT rev FROM document d WHERE d.reference = indexdoc.reference
         |   AND d.rev > indexdoc.doc_rev)
         | OR EXISTS (SELECT rev FROM word_suggestion w WHERE w.doc_ref = indexdoc.reference
         |   AND w.rev > coalesce(indexdoc.word_suggestion_rev, 0))
         | OR EXISTS (SELECT rev FROM metadata_correction m
         |   INNER JOIN metadata_correction_doc md on m.id = md.correction_id
         |   AND md.doc_ref = indexdoc.reference
         |   AND m.rev > coalesce(indexdoc.metadata_correction_rev, 0))
         | OR reindex
         | ORDER BY reference
       """.stripMargin
      .query[DocReference]
      .to[Seq]
      .transact(transactor)
  }

  def isContentUpdated(docRef: DocReference): Task[Boolean] = {
    sql"""SELECT indexdoc.reference
         | FROM indexed_document AS indexdoc
         | WHERE indexdoc.reference = ${docRef.ref}
         | AND EXISTS (SELECT rev FROM document d WHERE d.reference = indexdoc.reference
         |   AND d.rev > indexdoc.doc_rev)
         | OR EXISTS (SELECT rev FROM word_suggestion w WHERE w.doc_ref = indexdoc.reference
         |   AND w.rev > coalesce(indexdoc.word_suggestion_rev, 0))
         | OR reindex
       """.stripMargin
      .query[DocReference]
      .option
      .transact(transactor)
      .map(_.isDefined)
  }

  def getPage(docRev: DocRev, pageNumber: Int): Task[Option[DbPage]] =
    sql"""SELECT page.id, doc_rev, index, width, height, start_offset
         | FROM page
         | INNER JOIN document ON page.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND page.index = $pageNumber
       """.stripMargin
      .query[DbPage]
      .option
      .transact(transactor)

  def getPage(pageId: PageId): Task[DbPage] =
    sql"""SELECT page.id, doc_rev, index, width, height, start_offset
         | FROM page
         | INNER JOIN document ON page.doc_rev = document.rev
         | WHERE page.id = ${pageId.id}
       """.stripMargin
      .query[DbPage]
      .unique
      .transact(transactor)

  def getPageByWordOffset(docRev: DocRev, wordOffset: Int): Task[Option[DbPage]] =
    sql"""SELECT page.id, page.doc_rev, page.index, page.width, page.height, page.start_offset
         | FROM page
         | INNER JOIN document ON page.doc_rev = document.rev
         | INNER JOIN word ON word.doc_rev = document.rev
         | INNER JOIN row ON row.id = word.row_id AND row.page_id = page.id
         | WHERE document.rev = ${docRev.rev}
         | AND word.start_offset = $wordOffset
       """.stripMargin
      .query[DbPage]
      .option
      .transact(transactor)

  def getPages(docRev: DocRev): Task[Seq[DbPage]] =
    sql"""SELECT page.id, page.doc_rev, page.index, page.width, page.height, page.start_offset
         | FROM page
         | WHERE page.doc_rev = ${docRev.rev}
         | ORDER BY page.index
       """.stripMargin
      .query[DbPage]
      .to[Seq]
      .transact(transactor)

  def getRow(docRev: DocRev, pageNumber: Int, rowIndex: Int): Task[Option[DbRow]] =
    sql"""SELECT row.id, row.page_id, row.index, row.lft, row.top, row.width, row.height
         | FROM row
         | INNER JOIN page ON row.page_id = page.id
         | INNER JOIN document ON page.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND page.index = $pageNumber
         | AND row.index = $rowIndex
       """.stripMargin
      .query[DbRow]
      .option
      .transact(transactor)

  def getRowByStartOffset(docRev: DocRev, startOffset: Int): Task[Option[DbRow]] =
    sql"""SELECT row.id, row.page_id, row.index, row.lft, row.top, row.width, row.height
         | FROM row
         | INNER JOIN word ON row.id = word.row_id
         | INNER JOIN document ON word.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND word.start_offset = $startOffset
       """.stripMargin
      .query[DbRow]
      .option
      .transact(transactor)

  def getRowByEndOffset(docRev: DocRev, endOffset: Int): Task[Option[DbRow]] =
    sql"""SELECT row.id, row.page_id, row.index, row.lft, row.top, row.width, row.height
         | FROM row
         | INNER JOIN word ON row.id = word.row_id
         | INNER JOIN document ON word.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND word.start_offset = (SELECT max(w2.start_offset) FROM word w2
         |   WHERE w2.start_offset < $endOffset
         |   AND w2.doc_rev = document.rev)
       """.stripMargin
      .query[DbRow]
      .option
      .transact(transactor)

  def getRowsByStartAndEndOffset(docRev: DocRev, startOffset: Int, endOffset: Int): Task[Seq[DbRow]] =
    sql"""SELECT row.id, row.page_id, row.index, row.lft, row.top, row.width, row.height
         | FROM row
         | INNER JOIN row AS start_row ON row.page_id = start_row.page_id AND row.index >= start_row.index
         | INNER JOIN row AS end_row ON row.page_id = end_row.page_id AND row.index <= end_row.index
         | INNER JOIN word AS start_word ON start_row.id = start_word.row_id
         | INNER JOIN word AS end_word ON end_row.id = end_word.row_id
         | INNER JOIN document ON start_word.doc_rev = document.rev AND end_word.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND start_word.start_offset = $startOffset
         | AND end_word.start_offset = (SELECT max(w2.start_offset) FROM word w2
         |   WHERE w2.start_offset < $endOffset
         |   AND w2.doc_rev = document.rev)
         | ORDER BY row.page_id, row.index
       """.stripMargin
      .query[DbRow]
      .to[Seq]
      .transact(transactor)

  def getRow(rowId: RowId): Task[DbRow] =
    sql"""SELECT row.id, page_id, row.index, lft, top, row.width, row.height
         | FROM row
         | WHERE row.id = ${rowId.id}
       """.stripMargin
      .query[DbRow]
      .unique
      .transact(transactor)

  def getWord(docRev: DocRev, offset: Int): Task[Option[DbWord]] =
    sql"""SELECT word.id, doc_rev, row_id, start_offset, end_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | INNER JOIN document ON word.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND word.start_offset = $offset
       """.stripMargin
      .query[DbWord]
      .option
      .transact(transactor)

  def getWord(wordId: WordId): Task[DbWord] =
    sql"""SELECT word.id, doc_rev, row_id, start_offset, end_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | WHERE word.id = ${wordId.id}
       """.stripMargin
      .query[DbWord]
      .unique
      .transact(transactor)

  def getWordsInRow(docRev: DocRev, offset: Int): Task[Seq[DbWord]] =
    sql"""SELECT word.id, word.doc_rev, word.row_id, word.start_offset, word.end_offset, word.hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | INNER JOIN word AS w2 on w2.row_id = word.row_id
         | WHERE w2.doc_rev = ${docRev.rev}
         |   AND w2.start_offset = (SELECT MIN(w3.start_offset) FROM word w3
         |   WHERE w3.doc_rev = ${docRev.rev}
         |   AND w3.start_offset >= $offset)
         | ORDER BY word.start_offset
       """.stripMargin
      .query[DbWord]
      .to[Seq]
      .transact(transactor)

  def getQuery(queryId: QueryId): Task[DbQuery] =
    sql"""SELECT query.id, username, ip, executed, criteria, query, sort, first_result, max_result, result_count
         | FROM query
         | WHERE query.id = ${queryId.id}
       """.stripMargin
      .query[DbQuery]
      .unique
      .transact(transactor)

  def getQueriesSince(since: Instant): Task[Seq[DbQuery]] =
    sql"""SELECT query.id, username, ip, executed, criteria, query, sort, first_result, max_result, result_count
         | FROM query
         | WHERE query.executed >= $since
       """.stripMargin
      .query[DbQuery]
      .to[Seq]
      .transact(transactor)

  private val deleteAllWords: Task[Int] =
    sql"""DELETE FROM word""".update.run.transact(transactor)

  private val deleteAllRows: Task[Int] =
    sql"""DELETE FROM row""".update.run.transact(transactor)

  private val deleteAllPages: Task[Int] =
    sql"""DELETE FROM page""".update.run.transact(transactor)

  private val deleteAllDocuments: Task[Int] =
    sql"""DELETE FROM document""".update.run.transact(transactor)

  private val deleteAllIndexedDocuments: Task[Int] =
    sql"""DELETE FROM indexed_document""".update.run.transact(transactor)

  private val deleteAllQueries: Task[Int] =
    sql"""DELETE FROM query""".update.run.transact(transactor)

  private[service] val deleteAll: Task[Int] =
    for {
      _ <- deleteAllQueries
      _ <- deleteAllWords
      _ <- deleteAllRows
      _ <- deleteAllPages
      count <- deleteAllDocuments
      _ <- deleteAllIndexedDocuments
    } yield count
}

object SearchRepo {
  val live: ZLayer[Transactor[Task], Nothing, SearchRepo] =
    ZLayer {
      for {
        transactor <- ZIO.service[Transactor[Task]]
      } yield SearchRepo(transactor)
    }
}
