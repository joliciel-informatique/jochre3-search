package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.{
  AddAlternativesFilter,
  AddDocumentReferenceFilter,
  AddNewlineMarkerFilter,
  AddPageMarkerFilter,
  HyphenationFilter
}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForIndex(locale: Locale, languageSpecificFilters: Option[LanguageSpecificFilters] = None)
    extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val indexingHelper = IndexingHelper()

  private val maybePostTokenizationFilter = languageSpecificFilters.flatMap(_.postTokenizationFilterForIndex)
  private def postTokenizationFilter(tokens: TokenStream): TokenStream = {
    maybePostTokenizationFilter.map(_(tokens)).getOrElse(tokens)
  }

  override def finalFilter(tokens: TokenStream): TokenStream = (addDocumentReferenceFilter(_))
    .andThen(regexTokenizerFilter)
    .andThen(textNormalizingFilter)
    .andThen(lowercaseFilter)
    .andThen(postTokenizationFilter)
    .andThen(addPageMarkerFilter)
    .andThen(addNewlineMarkerFilter)
    .andThen(hyphenationFilter)
    .andThen(addAlternativesFilter)
    .andThen(textNormalizingFilter) // Normalize again after the alternatives have been added
    .andThen(ignorePunctuationFilter)
    .andThen(removeDuplicatesFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final"))
    .apply(tokens)

  def addDocumentReferenceFilter(tokens: TokenStream): TokenStream =
    new AddDocumentReferenceFilter(tokens, indexingHelper)

  def hyphenationFilter(tokens: TokenStream): TokenStream = new HyphenationFilter(tokens, indexingHelper)
  def addAlternativesFilter(tokens: TokenStream): TokenStream = new AddAlternativesFilter(tokens, indexingHelper)
  def addNewlineMarkerFilter(tokens: TokenStream): TokenStream = new AddNewlineMarkerFilter(tokens, indexingHelper)
  def addPageMarkerFilter(tokens: TokenStream): TokenStream = new AddPageMarkerFilter(tokens, indexingHelper)
}
