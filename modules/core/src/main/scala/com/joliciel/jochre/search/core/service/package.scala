package com.joliciel.jochre.search.core

import java.time.Instant

package object service {
  private[service] case class DocId(id: Long) extends AnyVal

  private[service] case class DbDocument(id: DocId, ref: DocReference, created: Instant)

  private[service] case class PageId(id: Long) extends AnyVal
  private[service] case class DbPage(id: PageId, docId: DocId, index: Int, width: Int, height: Int)

  private[service] case class RowId(id: Long) extends AnyVal
  private[service] case class DbRow(id: RowId, pageId: PageId, index: Int, left: Int, top: Int, width: Int, height: Int)

  private[service] case class WordId(id: Long) extends AnyVal
  private[service] case class DbWord(
      id: WordId,
      docId: DocId,
      rowId: RowId,
      offset: Int,
      hyphenatedOffset: Option[Int],
      left: Int,
      top: Int,
      width: Int,
      height: Int
  )
}
