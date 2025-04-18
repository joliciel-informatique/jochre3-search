package com.joliciel.jochre.search.core.service

import cats.implicits._
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.{DocReference, SearchCriterion, Sort}
import doobie._
import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import doobie.util.meta.Meta
import io.circe.generic.semiauto._
import zio._
import zio.interop.catz._

import java.time.Instant
import io.circe.Decoder
import io.circe.Encoder
import com.joliciel.jochre.search.core.IndexField

private[service] case class SearchRepo(transactor: Transactor[Task]) extends DoobieSupport {
  given Meta[DocumentStatusCode] =
    pgEnumStringOpt("document_status", DocumentStatusCode.fromEnum, DocumentStatusCode.toEnum)

  given Meta[ReindexType] =
    pgEnumStringOpt("reindex_type", ReindexType.fromEnum, ReindexType.toEnum)

  val reindexNo: ReindexType = ReindexType.No
  val reindexYes: ReindexType = ReindexType.Yes
  val reindexMetadataOnly: ReindexType = ReindexType.MetadataOnly

  def upsertIndexedDocument(
      ref: DocReference,
      docRev: DocRev,
      wordSuggestionRev: Option[WordSuggestionRev],
      metadataCorrections: Seq[DbMetadataCorrection],
      reindex: ReindexType
  ): Task[Int] = {
    val actions = for {
      result <- upsertIndexedDocumentInternal(ref, docRev, wordSuggestionRev, reindex)
      _ <- sql"DELETE FROM indexed_document_correction WHERE reference = ${ref.ref}".update.run
      _ <- insertIndexedDocumentCorrections(ref, metadataCorrections)
    } yield result
    actions.transact(transactor)
  }

  private def upsertIndexedDocumentInternal(
      ref: DocReference,
      docRev: DocRev,
      wordSuggestionRev: Option[WordSuggestionRev],
      reindex: ReindexType
  ): doobie.ConnectionIO[Int] =
    sql"""INSERT INTO indexed_document (reference, doc_rev, word_suggestion_rev, reindex)
         | VALUES (${ref.ref}, ${docRev.rev}, ${wordSuggestionRev.map(_.rev)}, $reindex)
         | ON CONFLICT (reference)
         | DO UPDATE SET doc_rev=${docRev.rev},
         |   word_suggestion_rev=${wordSuggestionRev.map(_.rev)},
         |   reindex=$reindex,
         |   index_time=CURRENT_TIMESTAMP
         | """.stripMargin.update.run

  private def insertIndexedDocumentCorrections(
      ref: DocReference,
      metadataCorrections: Seq[DbMetadataCorrection]
  ): doobie.ConnectionIO[Seq[Int]] =
    metadataCorrections.map { metadataCorrection =>
      sql"""INSERT INTO indexed_document_correction (reference, field, metadata_correction_rev)
           | VALUES (${ref.ref}, ${metadataCorrection.field.entryName}, ${metadataCorrection.rev.rev})
         """.stripMargin.update.run
    }.sequence

  def insertDocument(ref: DocReference, username: String, ipAddress: Option[String]): Task[DocRev] = {
    for {
      docRev <- insertDocumentInternal(ref, username, ipAddress)
      _ <- upsertIndexedDocument(ref, DocRev(0), None, Seq.empty, reindex = ReindexType.No)
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

  def markForReindex(docRef: DocReference, metaDataOnly: Boolean = false): Task[Int] = {
    val reindexType: ReindexType = if (metaDataOnly) { ReindexType.MetadataOnly }
    else { ReindexType.Yes }
    sql"""UPDATE indexed_document d1 SET reindex=$reindexType
          | WHERE reference=${docRef.ref}
       """.stripMargin.update.run
      .transact(transactor)
  }
  def markAllForReindex(metaDataOnly: Boolean = false): Task[Int] = {
    val reindexType: ReindexType = if (metaDataOnly) { ReindexType.MetadataOnly }
    else { ReindexType.Yes }
    sql"""UPDATE indexed_document d1 SET reindex=$reindexType
       """.stripMargin.update.run
      .transact(transactor)
  }
  def unmarkForReindex(docRef: DocReference): Task[Int] = {
    sql"""UPDATE indexed_document d1 SET reindex=$reindexNo
         | WHERE reference=${docRef.ref}
       """.stripMargin.update.run
      .transact(transactor)
  }

  def updateDocumentStatus(docRev: DocRev, status: DocumentStatus): Task[Int] = {
    val statusCode = status.code
    val failureReason = status match {
      case DocumentStatus.Failed(reason) => Some(reason)
      case _                             => None
    }
    sql"""UPDATE document
         | SET status=$statusCode,
         | failure_reason=$failureReason,
         | status_updated=CURRENT_TIMESTAMP
         | WHERE rev=${docRev.rev}""".stripMargin.update.run
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

  given Decoder[IndexField] = deriveDecoder[IndexField]
  given Encoder[IndexField] = deriveEncoder[IndexField]
  given Decoder[SearchCriterion] = deriveDecoder[SearchCriterion]
  given Encoder[SearchCriterion] = deriveEncoder[SearchCriterion]

  given Meta[SearchCriterion] = new Meta(pgDecoderGet, pgEncoderPut)

  given Decoder[Sort] = deriveDecoder[Sort]
  given Encoder[Sort] = deriveEncoder[Sort]
  given Meta[Sort] = new Meta(pgDecoderGet, pgEncoderPut)

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
    sql"""SELECT rev, reference, username, ip, created, status, failure_reason, status_updated
         | FROM document d1
         | WHERE d1.reference = ${ref.ref}
         | AND d1.rev = (SELECT MAX(rev) FROM document d2 WHERE d2.reference = d1.reference)
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getDocument(docRev: DocRev): Task[DbDocument] =
    sql"""SELECT rev, reference, username, ip, created, status, failure_reason, status_updated
         | FROM document
         | WHERE document.rev = ${docRev.rev}
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getIndexedDocument(ref: DocReference): Task[Option[DbIndexedDocument]] =
    sql"""SELECT reference, doc_rev, word_suggestion_rev, reindex, index_time
         | FROM indexed_document
         | WHERE reference = ${ref.ref} 
       """.stripMargin
      .query[DbIndexedDocument]
      .option
      .transact(transactor)

  def getIndexedDocumentCorrections(ref: DocReference): Task[Seq[DbIndexedDocumentCorrection]] =
    sql"""SELECT reference, field, metadata_correction_rev
         | FROM indexed_document_correction
         | WHERE reference = ${ref.ref}
         | ORDER BY field
       """.stripMargin
      .query[DbIndexedDocumentCorrection]
      .to[Seq]
      .transact(transactor)

  def getDocumentsToReindex(): Task[Seq[DocReference]] = {
    val completeStatus: DocumentStatusCode = DocumentStatusCode.Complete

    // Condition 1 (very rare): the document has been completely saved to the database
    // but the system was stopped before it got indexed.
    // Condition 2: the latest word suggestion hasn't yet been taken into account
    // Condition 3: the latest metadata correction for a given field hasn't yet been taken into account
    // Condition 4: the document has been marked for re-index (new alto or new metadata)
    sql"""SELECT indexdoc.reference
         | FROM indexed_document AS indexdoc
         | WHERE (
         |   EXISTS (SELECT rev FROM document d WHERE d.reference = indexdoc.reference
         |     AND d.status = $completeStatus
         |     AND d.rev > indexdoc.doc_rev)
         |   OR EXISTS (SELECT rev FROM word_suggestion w WHERE w.doc_ref = indexdoc.reference
         |     AND w.rev > coalesce(indexdoc.word_suggestion_rev, 0))
         |   OR EXISTS (
         |     SELECT mc.rev FROM metadata_correction mc
         |     INNER JOIN metadata_correction_doc mcd ON mc.id = mcd.correction_id
         |       AND mcd.doc_ref = indexdoc.reference
         |     WHERE mc.rev = (
         |       SELECT MAX(mc2.rev) FROM metadata_correction mc2
         |       INNER JOIN metadata_correction_doc mcd2 ON mc2.id = mcd2.correction_id
         |       WHERE mc2.field = mc.field
         |         AND mcd2.doc_ref = indexdoc.reference
         |     )
         |     AND (
         |       NOT EXISTS (
         |         SELECT idc.metadata_correction_rev
         |         FROM indexed_document_correction idc
         |         WHERE idc.reference = indexdoc.reference AND idc.field = mc.field
         |       )
         |       OR mc.rev > (
         |         SELECT idc.metadata_correction_rev
         |         FROM indexed_document_correction idc
         |         WHERE idc.reference = indexdoc.reference AND idc.field = mc.field
         |       )
         |     )
         |   )
         |   OR reindex = $reindexYes
         |   OR reindex = $reindexMetadataOnly
         | )
         | ORDER BY reference
       """.stripMargin
      .query[DocReference]
      .to[Seq]
      .transact(transactor)
  }

  def isContentUpdated(docRef: DocReference): Task[Boolean] = {
    val completeStatus: DocumentStatusCode = DocumentStatusCode.Complete

    sql"""SELECT indexdoc.reference
         | FROM indexed_document AS indexdoc
         | WHERE indexdoc.reference = ${docRef.ref}
         | AND (
         |   EXISTS (SELECT rev FROM document d WHERE d.reference = indexdoc.reference
         |     AND d.status = $completeStatus
         |     AND d.rev > indexdoc.doc_rev)
         |   OR EXISTS (SELECT rev FROM word_suggestion w WHERE w.doc_ref = indexdoc.reference
         |     AND w.rev > coalesce(indexdoc.word_suggestion_rev, 0))
         |   OR reindex = $reindexYes
         | )
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

  def getNonEmptyPages(docRev: DocRev): Task[Seq[DbPage]] =
    getPages(docRev).map { pages =>
      val (nonEmptyPages, _) = pages.foldLeft(Seq.empty[DbPage] -> -1) { case ((nonEmptyPages, lastOffset), page) =>
        if (page.offset > lastOffset) {
          (nonEmptyPages :+ page, page.offset)
        } else {
          (nonEmptyPages.init :+ page, page.offset)
        }
      }
      nonEmptyPages
    }

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
         | WHERE word.doc_rev = ${docRev.rev}
         | AND word.start_offset = (SELECT MIN(w2.start_offset) FROM word w2
         |   WHERE w2.start_offset >= $offset
         |   AND w2.doc_rev = ${docRev.rev})
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

  def markUnderwayAsFailedAtStartup(): Task[Int] = {
    val failedCode: DocumentStatusCode = DocumentStatusCode.Failed
    val failureReason: String = "Document analysis underway at startup"
    val underwayCode: DocumentStatusCode = DocumentStatusCode.Underway

    val actions = for {
      _ <-
        sql"""UPDATE indexed_document
             | SET reindex=$reindexYes
             | WHERE reference IN (
             |   SELECT reference FROM document WHERE document.status=$underwayCode
             | )
             | """.stripMargin.update.run
      result <-
        sql"""UPDATE document
             | SET status=$failedCode,
             | failure_reason=$failureReason,
             | status_updated=CURRENT_TIMESTAMP
             | WHERE document.status=$underwayCode
       """.stripMargin.update.run
    } yield result
    actions.transact(transactor)
  }

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
