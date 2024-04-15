package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.DocReference
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import zio._
import zio.interop.catz._

private[service] case class SearchRepo(transactor: Transactor[Task]) {
  def insertDocument(ref: DocReference): Task[DocRev] =
    sql"""INSERT INTO document (reference)
         | VALUES (${ref.ref})
         | RETURNING rev
         | """.stripMargin
      .query[DocRev]
      .unique
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
    sql"""INSERT INTO word(doc_rev, row_id, start_offset, hyphenated_offset, lft, top, width, height)
         | values (${docRev.rev}, ${rowId.id}, $offset, $hyphenatedOffset, ${word.left}, ${word.top}, ${word.width}, ${word.height})
         | RETURNING id
       """.stripMargin
      .query[WordId]
      .unique
      .transact(transactor)

  def getDocument(ref: DocReference): Task[DbDocument] =
    sql"""SELECT rev, reference, created
         | FROM document d1
         | WHERE d1.reference = ${ref.ref}
         | AND d1.rev = (SELECT MAX(rev) FROM document d2 WHERE d2.reference = d1.reference)
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getDocument(docRev: DocRev): Task[DbDocument] =
    sql"""SELECT rev, reference, created
         | FROM document
         | WHERE document.rev = ${docRev.rev}
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

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
    sql"""SELECT word.id, doc_rev, row_id, start_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | INNER JOIN document ON word.doc_rev = document.rev
         | WHERE document.rev = ${docRev.rev}
         | AND word.start_offset = $offset
       """.stripMargin
      .query[DbWord]
      .option
      .transact(transactor)

  def getWord(wordId: WordId): Task[DbWord] =
    sql"""SELECT word.id, doc_rev, row_id, start_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | WHERE word.id = ${wordId.id}
       """.stripMargin
      .query[DbWord]
      .unique
      .transact(transactor)

  private val deleteAllWords: Task[Int] =
    sql"""DELETE FROM word""".update.run.transact(transactor)

  private val deleteAllRows: Task[Int] =
    sql"""DELETE FROM row""".update.run.transact(transactor)

  private val deleteAllPages: Task[Int] =
    sql"""DELETE FROM page""".update.run.transact(transactor)

  private val deleteAllDocuments: Task[Int] =
    sql"""DELETE FROM document""".update.run.transact(transactor)

  private[service] val deleteAll: Task[Int] =
    for {
      _ <- deleteAllWords
      _ <- deleteAllRows
      _ <- deleteAllPages
      count <- deleteAllDocuments
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
