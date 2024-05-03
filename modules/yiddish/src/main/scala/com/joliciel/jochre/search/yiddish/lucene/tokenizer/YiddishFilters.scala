package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LanguageSpecificFilters
import org.apache.lucene.analysis.TokenStream
import zio.ZLayer

object YiddishFilters extends LanguageSpecificFilters {
  override val postTokenizationFilterForSearch: Option[TokenStream => TokenStream] = Some { input: TokenStream =>
    val reverseTransliterator = new ReverseTransliterator(input)
    val removeQuoteInAbbreviationFilter = new RemoveQuoteInAbbreviationFilter(reverseTransliterator)
    removeQuoteInAbbreviationFilter
  }

  override val postTokenizationFilterForIndex: Option[TokenStream => TokenStream] = Some { input: TokenStream =>
    new RemoveQuoteInAbbreviationFilter(input)
  }

  val live: ZLayer[Any, Throwable, LanguageSpecificFilters] = ZLayer.succeed(YiddishFilters)
}
