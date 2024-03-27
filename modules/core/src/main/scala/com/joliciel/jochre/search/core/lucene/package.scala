package com.joliciel.jochre.search.core

import enumeratum._

package object lucene {
  private[lucene] case class Token(value: String, start: Int, end: Int, score: Float)

  case class IndexTerm(text: String, start: Int, end: Int, position: Int) {
    def token: Token = Token(text, start, end, 1.0f)
  }

  sealed trait LuceneField extends EnumEntry {
    def isMultiValue: Boolean = false
  }

  private[lucene] object LuceneField extends Enum[LuceneField] {
    val values: IndexedSeq[LuceneField] = findValues

    case object Reference extends LuceneField

    case object Revision extends LuceneField

    case object Author extends LuceneField

    case object Title extends LuceneField

    case object Text extends LuceneField

    case object IndexTime extends LuceneField
  }

  private[lucene] val PAGE_TOKEN = "⚅"
  private[lucene] val NEWLINE_TOKEN = "⏎"
}
