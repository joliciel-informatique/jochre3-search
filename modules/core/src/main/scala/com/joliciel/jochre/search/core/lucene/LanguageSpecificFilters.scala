package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.analysis.TokenStream

trait LanguageSpecificFilters {
  def postTokenizationFilterForSearch: Option[TokenStream => TokenStream]

  def postTokenizationFilterForIndex: Option[TokenStream => TokenStream]
}
