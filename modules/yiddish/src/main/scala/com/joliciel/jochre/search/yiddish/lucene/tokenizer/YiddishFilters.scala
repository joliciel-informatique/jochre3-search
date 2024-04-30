package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LanguageSpecificFilters
import org.apache.lucene.analysis.TokenStream
import zio.ZLayer

object YiddishFilters extends LanguageSpecificFilters {
  override val postTokenizationFilter: Option[TokenStream => TokenStream] = Some { input: TokenStream =>
    new ReverseTransliterator(input)
  }

  val live: ZLayer[Any, Throwable, LanguageSpecificFilters] = ZLayer.succeed(YiddishFilters)
}
