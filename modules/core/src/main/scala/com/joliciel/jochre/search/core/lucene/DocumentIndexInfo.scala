package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.ocr.core.model.SpellingAlternative

case class DocumentIndexInfo(
    pageOffsets: Set[Int],
    newlineOffsets: Set[Int],
    hyphenatedWordOffsets: Set[Int],
    offsetToAlternativeMap: Map[Int, Seq[SpellingAlternative]]
) {
  def getAlternatives(offset: Int): Seq[SpellingAlternative] = {
    offsetToAlternativeMap.getOrElse(offset, Seq.empty)
  }
}
