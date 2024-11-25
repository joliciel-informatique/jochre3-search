package com.joliciel.jochre.search.core.text

import com.joliciel.jochre.ocr.core.model.{SpellingAlternative, Word}
import org.apache.lucene.analysis.TokenStream

trait LanguageSpecificFilters {
  def postTokenizationFilterForSearch: Option[TokenStream => TokenStream]

  def postTokenizationFilterForIndex: Option[TokenStream => TokenStream]

  def getAlternatives(word: String): Seq[SpellingAlternative]

  def breakWord(word: Word): Seq[Word]
}

object LanguageSpecificFilters {
  val default: LanguageSpecificFilters = new LanguageSpecificFilters {
    override def postTokenizationFilterForSearch: Option[TokenStream => TokenStream] = None

    override def postTokenizationFilterForIndex: Option[TokenStream => TokenStream] = None

    override def getAlternatives(word: String): Seq[SpellingAlternative] = Seq.empty

    override def breakWord(word: Word): Seq[Word] = Seq(word)
  }
}
