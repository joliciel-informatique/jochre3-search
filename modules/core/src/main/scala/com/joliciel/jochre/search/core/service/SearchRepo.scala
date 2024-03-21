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
  def insertDocument(ref: DocReference): Task[DocId] =
    sql"""INSERT INTO document (ref)
         | VALUES (${ref.ref})
         | RETURNING id
         | """.stripMargin
      .query[DocId]
      .unique
      .transact(transactor)

  def insertPage(docId: DocId, page: Page): Task[PageId] =
    sql"""INSERT INTO page (doc_id, index, width, height)
        | VALUES (${docId.id}, ${page.physicalPageNumber}, ${page.width}, ${page.height})
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

  def insertWord(docId: DocId, rowId: RowId, offset: Int, hyphenatedOffset: Option[Int], word: Word): Task[WordId] =
    sql"""INSERT INTO word(doc_id, row_id, start_offset, hyphenated_offset, lft, top, width, height)
         | values (${docId.id}, ${rowId.id}, $offset, $hyphenatedOffset, ${word.left}, ${word.top}, ${word.width}, ${word.height})
         | RETURNING id
       """.stripMargin
      .query[WordId]
      .unique
      .transact(transactor)

  def getDocument(ref: DocReference): Task[DbDocument] =
    sql"""SELECT id, ref, created
         | FROM document
         | WHERE document.ref = ${ref.ref}
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getDocument(docId: DocId): Task[DbDocument] =
    sql"""SELECT id, ref, created
         | FROM document
         | WHERE document.id = ${docId.id}
       """.stripMargin
      .query[DbDocument]
      .unique
      .transact(transactor)

  def getPage(ref: DocReference, pageNumber: Int): Task[DbPage] =
    sql"""SELECT page.id, doc_id, index, width, height
         | FROM page
         | INNER JOIN document ON page.doc_id = document.id
         | WHERE document.ref = ${ref.ref}
         | AND page.index = $pageNumber
       """.stripMargin
      .query[DbPage]
      .unique
      .transact(transactor)

  def getPage(pageId: PageId): Task[DbPage] =
    sql"""SELECT page.id, doc_id, index, width, height
         | FROM page
         | INNER JOIN document ON page.doc_id = document.id
         | WHERE page.id = ${pageId.id}
       """.stripMargin
      .query[DbPage]
      .unique
      .transact(transactor)

  def getRow(ref: DocReference, pageNumber: Int, rowIndex: Int): Task[DbRow] =
    sql"""SELECT row.id, page_id, row.index, lft, top, row.width, row.height
         | FROM row
         | INNER JOIN page ON row.page_id = page.id
         | INNER JOIN document ON page.doc_id = document.id
         | WHERE document.ref = ${ref.ref}
         | AND page.index = $pageNumber
         | AND row.index = $rowIndex
       """.stripMargin
      .query[DbRow]
      .unique
      .transact(transactor)

  def getRow(rowId: RowId): Task[DbRow] =
    sql"""SELECT row.id, page_id, row.index, lft, top, row.width, row.height
         | FROM row
         | WHERE row.id = ${rowId.id}
       """.stripMargin
      .query[DbRow]
      .unique
      .transact(transactor)

  def getWord(ref: DocReference, offset: Int): Task[DbWord] =
    sql"""SELECT word.id, doc_id, row_id, start_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
         | FROM word
         | INNER JOIN document ON word.doc_id = document.id
         | WHERE document.ref = ${ref.ref}
         | AND word.start_offset = $offset
       """.stripMargin
      .query[DbWord]
      .unique
      .transact(transactor)

  def getWord(wordId: WordId): Task[DbWord] =
    sql"""SELECT word.id, doc_id, row_id, start_offset, hyphenated_offset, word.lft, word.top, word.width, word.height
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
