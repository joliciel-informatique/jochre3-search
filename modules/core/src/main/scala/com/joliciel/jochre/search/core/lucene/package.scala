package com.joliciel.jochre.search.core

import enumeratum._

package object lucene {
  private[lucene] case class Token(value: String, start: Int, end: Int, score: Float)
  
  case class IndexTerm(text: String, start: Int, end: Int, position: Int) {
    def token: Token = Token(text, start, end, 1.0f)
  }
  
  sealed trait LuceneField extends EnumEntry {
    def name: String
    def isMultiValue: Boolean = false
  }

  private[lucene] object LuceneField extends Enum[LuceneField] {
    val values: IndexedSeq[LuceneField] = findValues

    case object Id extends LuceneField {
      val name: String = "id"
    }

    case object Author extends LuceneField {
      val name: String = "author"
    }

    case object Title extends LuceneField {
      val name: String = "title"
    }

    case object Text extends LuceneField {
      val name: String = "text"
    }

    case object IndexTime extends LuceneField {
      val name: String = "indexTime"
    }
  }
}
