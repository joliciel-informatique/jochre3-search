package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.tokenizer.{AddAlternativesFilter, RegexTokenizerFilter}
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer

import java.util.Locale

case class JochreAnalyzerForIndex(locale: Locale) extends Analyzer {
  private val alternativeHolder = AlternativeHolder()

  def addAlternatives(ref: DocReference, alternatives: Map[Int, Seq[SpellingAlternative]]): Unit =
    alternativeHolder.addAlternatives(ref, alternatives)
  def removeAlternatives(ref: DocReference): Unit = alternativeHolder.removeAlternatives(ref)

  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val source = new WhitespaceTokenizer();

    new TokenStreamComponents(source, finalFilter(source))
  }

  def finalFilter(tokens: TokenStream): TokenStream = (splitByRegexFilter(_))
    .andThen(addAlternativesFilter)
    .apply(tokens)

  def splitByRegexFilter(tokens: TokenStream): TokenStream = RegexTokenizerFilter(tokens, locale)
  def addAlternativesFilter(tokens: TokenStream): TokenStream = new AddAlternativesFilter(tokens, alternativeHolder)
}
