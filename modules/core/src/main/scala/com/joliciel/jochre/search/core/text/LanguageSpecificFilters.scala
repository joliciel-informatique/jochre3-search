package com.joliciel.jochre.search.core.text

import com.joliciel.jochre.ocr.core.model.{SpellingAlternative, Word}
import org.apache.lucene.analysis.TokenStream

import scala.util.matching.Regex

trait LanguageSpecificFilters {
  def postTokenizationFilterForSearch: Option[TokenStream => TokenStream]

  def postTokenizationFilterForIndex: Option[TokenStream => TokenStream]

  def getAlternatives(word: String): Seq[SpellingAlternative]

  def breakWord(word: Word): Seq[Word]

  def queryFindReplacePairs: Seq[(Regex, String)]

  /** Simplify text so that all possible ways of writing a certain character use the same encoding.
    */
  def simplifyText(text: String): String

  /** Normalize text for searching, e.g. by removing all diacritics.
    */
  def normalizeText(text: String): String
}

object LanguageSpecificFilters {
  val default: LanguageSpecificFilters = new LanguageSpecificFilters {
    override def postTokenizationFilterForSearch: Option[TokenStream => TokenStream] = None

    override def postTokenizationFilterForIndex: Option[TokenStream => TokenStream] = None

    override def getAlternatives(word: String): Seq[SpellingAlternative] = Seq.empty

    override def breakWord(word: Word): Seq[Word] = Seq(word)

    override def queryFindReplacePairs: Seq[(Regex, String)] = Seq.empty

    override def simplifyText(text: String): String = text

    override def normalizeText(text: String): String = text
  }
}
