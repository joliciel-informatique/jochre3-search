package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.{IndexField, Sort}
import enumeratum.{Enum, EnumEntry}

package object search {
  case class SizeResponse(size: Int)

  case class WordText(text: String)

  sealed trait SortKind extends EnumEntry {
    def toSort: Sort
  }
  object SortKind extends Enum[SortKind] {
    val values: IndexedSeq[SortKind] = findValues

    case object Score extends SortKind {
      override def toSort: Sort = Sort.Score
    }
    case object DateAscending extends SortKind {
      override def toSort: Sort = Sort.Field(IndexField.PublicationYearAsNumber, ascending = true)
    }
    case object DateDescending extends SortKind {
      override def toSort: Sort = Sort.Field(IndexField.PublicationYearAsNumber, ascending = false)
    }
    case object DocReference extends SortKind {
      override def toSort: Sort = Sort.Field(IndexField.Reference, ascending = true)
    }
  }
}
