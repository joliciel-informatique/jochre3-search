package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.{
  AddAlternativesFilter,
  AddDocumentReferenceFilter,
  AddNewlineMarkerFilter,
  AddPageMarkerFilter
}
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForIndex(locale: Locale) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val indexingHelper = IndexingHelper()

  override def finalFilter(tokens: TokenStream): TokenStream = (addDocumentReferenceFilter(_))
    .andThen(regexTokenizerFilter)
    .andThen(lowercaseFilter)
    .andThen(addAlternativesFilter)
    .andThen(addPageMarkerFilter)
    .andThen(addNewlineMarkerFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
    .apply(tokens)

  def addDocumentReferenceFilter(tokens: TokenStream): TokenStream =
    new AddDocumentReferenceFilter(tokens, indexingHelper)

  def addAlternativesFilter(tokens: TokenStream): TokenStream = new AddAlternativesFilter(tokens, indexingHelper)
  def addNewlineMarkerFilter(tokens: TokenStream): TokenStream = new AddNewlineMarkerFilter(tokens, indexingHelper)
  def addPageMarkerFilter(tokens: TokenStream): TokenStream = new AddPageMarkerFilter(tokens, indexingHelper)
}
